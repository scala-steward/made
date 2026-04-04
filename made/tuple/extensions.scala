package made.tuple

import scala.compiletime.ops.boolean.||
import scala.compiletime.ops.int.S
import scala.Tuple.Map

extension (tup: Tuple)
  inline def foreach(f: [t] => t => Unit): Unit = tup.map[[X] =>> Unit](f)

  inline def indices: Indices[tup.type] = Tuple.fromArray(Array.range(0, tup.size)).asInstanceOf[Indices[tup.type]]

  inline def mapOnly[T](using tup.type containsOnly T): MapOnly[T, tup.type] = tup

opaque type MapOnly[T, Tup <: Tuple] = Tup

object MapOnly:
  extension [T, Tup <: Tuple](mapOnly: MapOnly[T, Tup])
    inline def apply[F[_ <: T]](inline f: [t <: T] => t => F[t]): Map[Tup, [X] =>> F[X & T]] =
      (mapOnly: Tup).map[[X] =>> F[X & T]]([t] => (t: t) => f(t.asInstanceOf[t & T]))

type Indices[Tup <: Tuple] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => 0 *: IndicesAux[t, 1]

type IndicesAux[Tup <: Tuple, N <: Int] <: Tuple = Tup match
  case EmptyTuple => EmptyTuple
  case h *: t => N *: IndicesAux[t, S[N]]

type HasDuplicates[Tup <: Tuple] <: Boolean = Tup match
  case EmptyTuple => false
  case h *: t => Tuple.Contains[t, h] || HasDuplicates[t]
