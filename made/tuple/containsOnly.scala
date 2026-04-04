opaque infix type containsOnly[Tup <: Tuple, T] = Boolean

object containsOnly:

  type Loop[Tup <: Tuple, T] <: Boolean = Tup match
    case EmptyTuple => true
    case T *: tail => Loop[tail, T]
    case _ => false

  inline given [Tup <: Tuple, T] => (Loop[Tup, T] =:= true) => containsOnly[Tup, T] = true
