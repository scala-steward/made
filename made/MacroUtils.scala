package made

import made.annotation.{name, MetaAnnotation}

import scala.annotation.Annotation
import scala.quoted.*

// like ValueOf but without the implicit search and boxing
inline private[made] def singleValueOf[T <: scala.Singleton]: T = ${ singleValueOfImpl[T] }

private def singleValueOfImpl[T <: scala.Singleton: Type](using quotes: Quotes): Expr[T] =
  import quotes.reflect.*
  val term = TypeRepr.of[T] match
    case ConstantType(c: Constant) => Literal(c)
    case tp: TypeRef if tp <:< TypeRepr.of[Unit] => Literal(UnitConstant())
    case n: TermRef => Ref(n.termSymbol)
    case ts: ThisType => This(ts.classSymbol.get)
    case tp => report.errorAndAbort(s"Unsupported singleton type: ${tp.show}")
  term.asExprOf[T]

extension (comp: Expr.type)
  def ofOption[A: Type](opt: Option[Expr[A]])(using Quotes): Expr[Option[A]] = opt match
    case Some(expr) => '{ Some($expr) }
    case None => '{ None }

def stringToType(str: String)(using quotes: Quotes): Type[? <: String] =
  import quotes.reflect.*
  ConstantType(StringConstant(str)).asType.asInstanceOf[Type[? <: String]]

def typeToString[S <: String: Type](using quotes: Quotes): S =
  import quotes.reflect.*
  TypeRepr.of[S] match
    case ConstantType(StringConstant(str)) => str.asInstanceOf[S]
    case _ => report.errorAndAbort(s"Unsupported singleton type: ${Type.show[S]}")

def traverseTypes(tpes: List[Type[? <: AnyKind]])(using Quotes): Type[? <: Tuple] =
  val empty: Type[? <: Tuple] = Type.of[EmptyTuple]
  tpes.foldRight(empty):
    case ('[tpe], '[type acc <: Tuple; acc]) => Type.of[tpe *: acc]
    case (_, _) => wontHappen

def traverseTuple(tpe: Type[? <: Tuple])(using Quotes): List[Type[? <: AnyKind]] = tpe match
  case '[EmptyTuple] => Nil
  case '[t *: ts] => Type.of[t] :: traverseTuple(Type.of[ts])

def reportOnDuplicates(labels: Seq[(label: String, original: String)])(using Quotes): Unit =
  import quotes.reflect.*
  labels
    .groupMap(_.label)(_.original)
    .foreach: (label, originals) =>
      if originals.sizeIs > 1 then report.error(s"${originals.mkString(", ")} have the same @name: $label")

private[made] class MacroUtils[Q <: Quotes](using val quotes: Q):
  import quotes.reflect.*

  extension (symbol: Symbol)
    def hasAnnotationOf[AT <: Annotation: Type] =
      symbol.hasAnnotation(TypeRepr.of[AT].typeSymbol)

    def getAnnotationOf[AT <: Annotation: Type] =
      symbol.getAnnotation(TypeRepr.of[AT].typeSymbol).map(_.asExprOf[AT])

  def metaTypeOf(symbol: Symbol): Type[? <: Meta] =
    symbol.annotations
      .filter(_.tpe <:< TypeRepr.of[MetaAnnotation])
      .foldRight(TypeRepr.of[Meta])((annot, tpe) => AnnotatedType(tpe, annot))
      .asType
      .asInstanceOf[Type[? <: Meta]]

  def labelTypeOf(sym: Symbol, fallback: String): Type[? <: String] =
    val syms = Iterator(sym) ++ sym.allOverriddenSymbols
    val res = syms.find(_.hasAnnotationOf[name]).flatMap(_.getAnnotationOf[name])
    stringToType(res match
      case Some('{ new `name`($value) }) => value.valueOrAbort
      case _ => fallback)
