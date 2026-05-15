package made

import scala.annotation.{publicInBinary, tailrec, Annotation}
import scala.quoted.*

extension [M <: Tuple](self: { type Metadata = M })
  /**
   * Returns `true` if the mirror's `Metadata` tuple contains an entry with annotation `A`,
   * `false` otherwise.
   *
   * Transparent inline — the result is a singleton-typed `true` or `false` literal type, so
   * callers can use it directly in `inline if` to specialise code at compile time.
   * `A` must extend [[made.annotation.MetaAnnotation]].
   */
  transparent inline def hasAnnotation[A <: Annotation]: Boolean = ${ hasAnnotationImpl[A, M] }

  /**
   * Returns `Some(annotation)` if the mirror's `Metadata` tuple contains an entry with
   * annotation of type `A`, `None` otherwise.
   *
   * Transparent inline — the result is statically typed as `Some[A]` or `None.type`, enabling
   * compile-time dispatch via `inline match`.
   * The returned annotation instance provides access to annotation parameters
   * (e.g., `getAnnotation[JsonName].get.value`). `A` must extend
   * [[made.annotation.MetaAnnotation]].
   */
  transparent inline def getAnnotation[A <: Annotation]: Option[A] = ${ getAnnotationImpl[A, M] }

extension [L <: String](l: { type Label = L })
  /**
   * Returns the label of the mirror.
   */
  inline def label: L = compiletime.constValue[L]

extension [Ls <: Tuple](l: { type ElemLabels = Ls })
  /**
   * Returns the labels of the mirror's elements.
   */
  inline def elemLabels: Ls = compiletime.constValueTuple[Ls]

@publicInBinary private def getAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes)
  : Expr[Option[A]] =
  import quotes.reflect.*

  def walk(tpe: Type[? <: Tuple]): List[Type[? <: AnyKind]] = tpe match
    case '[EmptyTuple] => Nil
    case '[t *: ts] => Type.of[t] :: walk(Type.of[ts])
    case _ => Nil // abstract / non-reducible tuple: treat as if no annotations

  @tailrec def loop(tpes: List[Type[? <: AnyKind]]): Option[Expr[A]] = tpes match
    case Nil => None
    case head :: rest =>
      head match
        case '[t] =>
          TypeRepr.of[t] match
            case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => Some(annot.asExprOf[A])
            case _ => loop(rest)

  loop(walk(Type.of[M])) match
    case Some(annotExpr) => '{ Some($annotExpr) }
    case None => '{ None }

@publicInBinary private def hasAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes)
  : Expr[Boolean] =
  if getAnnotationImpl[A, M].isExprOf[Some[A]] then '{ true } else '{ false }
