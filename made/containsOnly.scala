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

  /** A constant map `[_] =>> C` makes every element `C`. Unifies even for abstract `Es`. */
  given [Es <: Tuple, C] => (Tuple.Map[Es, [_] =>> C] containsOnly C) = refl

  /** A covariant `F` gives `F[e] <: F[Any]` for every element (invariant `F` still needs `refl`). */
  given [Es <: Tuple, F[+_]] => (Tuple.Map[Es, F] containsOnly F[Any]) = refl

  import scala.language.implicitConversions

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Head[Tup], T] = _.asInstanceOf[T]

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Last[Tup], T] = _.asInstanceOf[T]

sealed trait containsOnlyLowPriority:
  given Tuple containsOnly Any = containsOnly.refl
