package made.tuple

import scala.compiletime.ops.boolean.||
import scala.compiletime.ops.int.S
import scala.Tuple.Map

extension (tup: Tuple)
  inline def foreach(f: [t] => t => Unit): Unit = tup.map[[X] =>> Unit](f)

  inline def indices: Indices[tup.type] =
    Tuple.fromArray(Array.range(0, tup.size)).asInstanceOf[Indices[tup.type]]

  // todo: we'd like o avoid the second type param
  inline def mapOnly[T, F[_ <: T]](
    f: [t <: T] => t => F[t],
  )(using tup.type containsOnly T,
  ): Map[tup.type, [X] =>> F[X & T]] =
    tup.map([t] => (t: t) => f(t.asInstanceOf[t & T]))

type Indices[Tup <: Tuple] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => 0 *: IndicesAux[t, 1]

type IndicesAux[Tup <: Tuple, N <: Int] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => N *: IndicesAux[t, S[N]]

type HasDuplicates[Tup <: Tuple] <: Boolean = Tup match
  case EmptyTuple => false
  case h *: t => Tuple.Contains[t, h] || HasDuplicates[t]
