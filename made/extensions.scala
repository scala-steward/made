package made

import scala.annotation.{publicInBinary, Annotation}
import scala.quoted.*

extension [M <: Tuple](self: { type Metadata = M })(using M containsOnly Meta)
  /**
   * Returns `true` if the mirror's `Metadata` tuple contains an entry with annotation `A`,
   * `false` otherwise.
   *
   * Transparent inline — the result is a singleton-typed `true` or `false` literal type, so
   * callers can use it directly in `inline if` to specialise code at compile time.
   * `A` must extend [[made.annotation.MetaAnnotation]].
   *
   * Requires evidence that `M` is a tuple of `Meta` (or `Meta @ann`) entries; the evidence
   * fails to summon when `M` is abstract, surfacing a compile error instead of silently
   * returning `false`.
   */
  transparent inline def hasAnnotation[A <: Annotation]: Boolean =
    ${ hasAnnotationImpl[A, M] }

  /**
   * Returns `Some(annotation)` if the mirror's `Metadata` tuple contains an entry with
   * annotation of type `A`, `None` otherwise.
   *
   * Transparent inline — the result is statically typed as `Some[A]` or `None.type`, enabling
   * compile-time dispatch via `inline match`.
   * The returned annotation instance provides access to annotation parameters
   * (e.g., `getAnnotation[JsonName].get.value`). `A` must extend
   * [[made.annotation.MetaAnnotation]].
   *
   * Requires evidence that `M` is a tuple of `Meta` (or `Meta @ann`) entries; the evidence
   * fails to summon when `M` is abstract, surfacing a compile error instead of silently
   * returning `None`.
   */
  transparent inline def getAnnotation[A <: Annotation]: Option[A] =
    ${ getAnnotationImpl[A, M] }

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
   * Per-element annotation presence check over a tuple of [[MadeElem]]s.
   *
   * For each `Ei` in `Es`, evaluates whether `Ei`'s `Metadata` tuple contains an annotation
   * of type `A`, returning a tuple of singleton-typed `true`/`false` literals of the same
   * arity as `Es`.
   *
   * Transparent inline — the result type narrows to e.g. `(true, false, true)`, enabling
   * compile-time dispatch in derivation logic.
   *
   * Requires evidence that `Es` is a tuple of [[MadeElem]]s.
   */
  transparent inline def hasAnnotations[A <: Annotation]: Tuple =
    ${ hasAnnotationsImpl[Es, A] }

  /**
   * Per-element annotation extraction over a tuple of [[MadeElem]]s.
   *
   * For each `Ei` in `Es`, returns `Some(annot)` when `Ei`'s `Metadata` contains an
   * annotation of type `A`, otherwise `None`. The result is a tuple of singleton-typed
   * `Some[A]` / `None.type` entries.
   *
   * Transparent inline — narrows per-element. Requires evidence that `Es` is a tuple of
   * [[MadeElem]]s.
   */
  transparent inline def getAnnotations[A <: Annotation]: Tuple =
    ${ getAnnotationsImpl[Es, A] }

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
  : Expr[Tuple] = Expr.ofTupleFromSeq:
  traverseTuple(Type.of[Es]).map:
    case '[type m <: Tuple; MadeElem { type Metadata = m }] => hasAnnotationImpl[A, m]
    case _ => Expr(false)

@publicInBinary private def getAnnotationsImpl[Es <: Tuple: Type, A <: Annotation: Type](using quotes: Quotes)
  : Expr[Tuple] = Expr.ofTupleFromSeq:
  traverseTuple(Type.of[Es]).map:
    case '[type m <: Tuple; MadeElem { type Metadata = m }] => getAnnotationImpl[A, m]
    case _ => Expr(None)
