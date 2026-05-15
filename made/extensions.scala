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

extension [Es <: Tuple](es: Es)(using Es containsOnly MadeElem)
  /**
   * Per-element [[hasAnnotation]] over a tuple of [[MadeElem]]s.
   */
  transparent inline def hasAnnotations[A <: Annotation]: Tuple = ${ hasAnnotationsImpl[Es, A] }

  /**
   * Per-element [[getAnnotation]] over a tuple of [[MadeElem]]s.
   */
  transparent inline def getAnnotations[A <: Annotation]: Tuple = ${ getAnnotationsImpl[Es, A] }

@publicInBinary private def getAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes)
  : Expr[Option[A]] =
  import quotes.reflect.*

  Expr.ofOption:
    traverseTuple(Type.of[M]).iterator
      .map(TypeRepr.of(using _))
      .collectFirst:
        case AnnotatedType(_, annot) if annot.tpe <:< TypeRepr.of[A] => annot.asExprOf[A]

@publicInBinary private def hasAnnotationImpl[A <: Annotation: Type, M <: Tuple: Type](using quotes: Quotes)
  : Expr[Boolean] = Expr(getAnnotationImpl[A, M].isExprOf[Some[A]])

@publicInBinary private def hasAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using quotes: Quotes)
  : Expr[Tuple] =
  import quotes.reflect.*
  Expr.ofTupleFromSeq:
    traverseTuple(Type.of[Es]).map:
      case '[h] =>
        TypeRepr.of[MadeElem.ExtractMeta[h]].dealias.asType match
          case '[type m <: Tuple; m] => hasAnnotationImpl[A, m]
          case _ => Expr(false)

@publicInBinary private def getAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using quotes: Quotes)
  : Expr[Tuple] =
  import quotes.reflect.*
  Expr.ofTupleFromSeq:
    traverseTuple(Type.of[Es]).map:
      case '[h] =>
        TypeRepr.of[MadeElem.ExtractMeta[h]].dealias.asType match
          case '[type m <: Tuple; m] => getAnnotationImpl[A, m]
          case _ => Expr(None)
