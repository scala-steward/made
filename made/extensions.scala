package made

import scala.annotation.{publicInBinary, Annotation}
import scala.quoted.*

extension [M <: Tuple](self: { type Metadata = M })(using M containsOnly Meta)
  /**
   * Returns `true` if the mirror's `Metadata` tuple contains an annotation of type `A`.
   *
   * Transparent inline - resolved entirely at compile time, no runtime cost.
   * `A` must extend [[made.annotation.MetaAnnotation]].
   */
  transparent inline def hasAnnotation[A <: Annotation]: Boolean = ${ hasAnnotationImpl[A, M] }

  /**
   * Returns `Some(annotation)` if the mirror's `Metadata` tuple contains an annotation
   * of type `A`, `None` otherwise.
   *
   * The returned annotation instance provides access to annotation parameters
   * (e.g., `getAnnotation[JsonName].get.value`). Transparent inline - resolved at compile time.
   * `A` must extend [[made.annotation.MetaAnnotation]].
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

extension (es: Tuple)(using es.type containsOnly { type Metadata <: Tuple })
  /**
   * Per-element [[hasAnnotation]] over a tuple whose entries each declare a `Metadata` type member
   * (e.g. a tuple of [[MadeElem]]s, [[GeneratedMadeElem]]s, or a singleton `Made` instance's
   * `Metadata` chain).
   */
  transparent inline def hasAnnotations[A <: Annotation]: Tuple.Map[es.type, [_] =>> Boolean] =
    ${ hasAnnotationsImpl[es.type, A] }

  /**
   * Per-element [[getAnnotation]] over a tuple whose entries each declare a `Metadata` type member.
   */
  transparent inline def getAnnotations[A <: Annotation]: Tuple.Map[es.type, [_] =>> Option[A]] =
    ${ getAnnotationsImpl[es.type, A] }

// $COVERAGE-OFF$
@publicInBinary private def getAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes)
  : Expr[Option[A]] =
  import quotes.reflect.*

  Expr.ofOption:
    traverseTuple(Type.of[M]).iterator
      .map(TypeRepr.of(using _))
      .collectFirst:
        case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A]

@publicInBinary private def hasAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using Quotes): Expr[Boolean] =
  Expr(getAnnotationImpl[A, M].isExprOf[Some[A]])

@publicInBinary private def hasAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using Quotes)
  : Expr[Tuple.Map[Es, [_] =>> Boolean]] = Expr
  .ofRefinedTuple:
    traverseTuple(Type.of[Es]).map:
      case '[type m <: Tuple; { type Metadata = m }] => hasAnnotationImpl[A, m]
  .asInstanceOf[Expr[Tuple.Map[Es, [_] =>> Boolean]]]

@publicInBinary private def getAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using Quotes)
  : Expr[Tuple.Map[Es, [_] =>> Option[A]]] = Expr
  .ofRefinedTuple:
    traverseTuple(Type.of[Es]).map:
      case '[type m <: Tuple; { type Metadata = m }] => getAnnotationImpl[A, m]
  .asInstanceOf[Expr[Tuple.Map[Es, [_] =>> Option[A]]]]

// $COVERAGE-ON$
