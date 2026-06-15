package made

import scala.annotation.unused

opaque infix type containsOnly[-Tup <: Tuple, +T] = Boolean //or true?

object containsOnly extends containsOnlyLowPriority:

  type Loop[Tup <: Tuple, T] <: Boolean = Tup match
    case EmptyTuple => true
    case T *: tail => Loop[tail, T]
    case _ => false

  def refl[Tup <: Tuple, T]: Tup containsOnly T = true

  inline given [Tup <: Tuple, T] => (ev: Loop[Tup, T] =:= true) => containsOnly[Tup, T] = true

  /**
   * Evidence for a tuple built by mapping a *constant* type function over another tuple:
   * every element is exactly `C`, so it `containsOnly C`. Sound and resolved by unification,
   * so it works even when the underlying tuple is abstract (e.g. a `Made` mirror's `Elems`),
   * where the structural [[Loop]] cannot reduce.
   */
  given [Es <: Tuple, C] => (Tuple.Map[Es, [_] =>> C] containsOnly C) = refl

  /**
   * Evidence for a tuple built by mapping a *covariant* type constructor `F` over another
   * tuple: every element is `F[someElem] <: F[Any]`, so it `containsOnly F[Any]`. The `F[+_]`
   * bound keeps this sound — it only fires for covariant functors. Resolved by unification, so
   * it works for abstract underlying tuples (e.g. `summonAll[Tuple.Map[m.ElemTypes, Show]]`
   * where `Show[+T]`). For an *invariant* `F` the relation does not hold, so an explicit
   * `(using containsOnly.refl)` is still required to assert the erasure-safe cast.
   */
  given [Es <: Tuple, F[+_]] => (Tuple.Map[Es, F] containsOnly F[Any]) = refl

  import scala.language.implicitConversions

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Head[Tup], T] = _.asInstanceOf[T]

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Last[Tup], T] = _.asInstanceOf[T]

sealed trait containsOnlyLowPriority:
  given Tuple containsOnly Any = containsOnly.refl
