package made

import scala.annotation.unused

opaque infix type containsOnly[Tup <: Tuple, T] = Boolean //or true?

object containsOnly:

  type Loop[Tup <: Tuple, T] <: Boolean = Tup match
    case EmptyTuple => true
    case T *: tail => Loop[tail, T]
    case _ => false

  def refl[Tup <: Tuple, T]: Tup containsOnly T = true

  inline given [Tup <: Tuple, T] => (ev: Loop[Tup, T] =:= true) => containsOnly[Tup, T] = true

  import scala.language.implicitConversions

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Head[Tup], T] = _.asInstanceOf[T]

  given [Tup <: Tuple, T] => (@unused ev: Tup containsOnly T) => Conversion[Tuple.Last[Tup], T] = _.asInstanceOf[T]
