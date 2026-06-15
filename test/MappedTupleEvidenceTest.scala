package made

import made.annotation.optionalParam

/**
 * A realistic product-codec derivation that maps over a `Made` mirror's tuples
 * (`ElemLabels`, `ElemTypes`, `elems`). Every `toArrayOf` / `mapAs` here used to require an
 * explicit `(using containsOnly.refl)` because the mapped tuples never reduce for an abstract
 * mirror; the path-dependent and mapped-tuple `containsOnly` givens now supply that evidence
 * automatically, so this whole derivation is refl-free.
 */
class MappedTupleEvidenceTest extends munit.FunSuite:
  import MappedTupleEvidenceTest.*

  test("product codec derivation summons all containsOnly evidence without containsOnly.refl") {
    val codec = MCodec.derived[User]

    assertEquals(codec.labels.toList, List("name", "age", "address"))
    assertEquals(codec.codecs.length, 3)
    assertEquals(codec.optionalFlags.toList, List(false, false, true))

    // age carries a constructor default; the per-field default mapping resolved it
    assertEquals(codec.defaults.length, 3)
    assertEquals(codec.defaults(1), Some(25))

    // the captured constructor still rebuilds a value end-to-end
    assertEquals(codec.ctor(Array("Bob", 30, Some("x"))), User("Bob", 30, Some("x")))
  }

  test("covariant functor maps resolve toArrayOf[F[Any]] with no witness") {
    // `Decoder[+T]` is covariant, so `Tuple.Map[ElemTypes, Decoder] containsOnly Decoder[Any]`
    // holds soundly and is derived automatically — no `containsOnly.refl` needed.
    assertEquals(Decoder.all[User].length, 3)
  }

object MappedTupleEvidenceTest:
  trait MCodec[T]

  object MCodec:
    given MCodec[String] = new MCodec[String] {}
    given MCodec[Int] = new MCodec[Int] {}
    given MCodec[Option[String]] = new MCodec[Option[String]] {}

    final class Product[T](
      val labels: Array[String],
      val codecs: Array[MCodec[Any]],
      val defaults: Array[Option[Any]],
      val optionalFlags: Array[Boolean],
      val ctor: Array[Any] => T,
    ) extends MCodec[T]

    inline def derived[T](using m: Made.Of[T]): Product[T] = inline m match
      case m: Made.ProductOf[T & scala.Product] => deriveProduct(m).asInstanceOf[Product[T]]

    inline def deriveProduct[T](m: Made.ProductOf[T]): Product[T] = Product[T](
      compiletime.constValueTuple[m.ElemLabels].toArrayOf[String],
      // `MCodec` is invariant, so `MCodec[Int]` is not a `MCodec[Any]`: this widening is an
      // erasure-safe cast, not a typed subtype relation, so it needs an explicit witness.
      // (A covariant `Show[+T]` would resolve here automatically via the `F[+_]` given.)
      compiletime.summonAll[Tuple.Map[m.ElemTypes, MCodec]].toArrayOf[MCodec[Any]](using containsOnly.refl),
      m.elems
        .mapAs[MadeFieldElem][[e <: MadeFieldElem] =>> Option[Any]]([e <: MadeFieldElem] => (elem: e) => elem.default)
        .toArrayOf[Option[Any]],
      // `hasAnnotations` is a whole-tuple macro: it needs the concrete `Elems` and yields a
      // precise `(false, false, true)` at expansion, but its declared type is the abstract
      // `Tuple`, so `toArrayOf[Boolean]` still needs an explicit witness here. (It cannot be
      // written as `mapAs(_.hasAnnotation)` — the per-element macro would see an abstract type.)
      m.elems.hasAnnotations[optionalParam].toArrayOf[Boolean](using containsOnly.refl),
      m.fromUnsafeArray,
    )

  trait Decoder[+T]

  object Decoder:
    given Decoder[String] = new Decoder[String] {}
    given Decoder[Int] = new Decoder[Int] {}
    given Decoder[Option[String]] = new Decoder[Option[String]] {}

    inline def all[T](using m: Made.Of[T]): Array[Decoder[Any]] = inline m match
      case m: Made.ProductOf[T & scala.Product] =>
        compiletime.summonAll[Tuple.Map[m.ElemTypes, Decoder]].toArrayOf[Decoder[Any]]

  case class User(name: String, age: Int = 25, @optionalParam address: Option[String])
