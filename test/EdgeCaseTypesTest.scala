package made

import made.annotation.transparent

class EdgeCaseTypesTest extends munit.FunSuite:
  import EdgeCaseTypesTest.*

  test("derives Product with opaque-type field") {
    val m = Made.derived[WithOpaque]
    val id *: name *: EmptyTuple = m.elems
    assertEquals(id.label, "id")
    assertEquals(name.label, "name")
    val v = m.fromTuple((Ids("u-1"), "Alice"))
    assertEquals(id.apply(v), Ids("u-1"))
  }

  test("derives Product with union-type field") {
    val m = Made.derived[WithUnion]
    val v *: EmptyTuple = m.elems
    assertEquals(v.label, "v")
    assertEquals(v.apply(WithUnion(42)).asInstanceOf[Int | String], 42)
    assertEquals(v.apply(WithUnion("x")).asInstanceOf[Int | String], "x")
  }

  test("derives Product with intersection-type field") {
    val m = Made.derived[WithIntersection]
    val v *: EmptyTuple = m.elems
    assertEquals(v.label, "v")
    val obj = new MarkA with MarkB
    assertEquals(v.apply(WithIntersection(obj)), obj)
  }

  test("derives Sum for GADT-style enum") {
    val m = Made.derived[Expr[Int]]
    assertEquals(m.ordinal(Expr.IntLit(1)), 0)
  }

  test("derives transparent wrapper over opaque type") {
    val m = Made.derived[OpaqueWrapper]
    val inner *: EmptyTuple = m.elems
    assertEquals(inner.apply(OpaqueWrapper(Ids("w-1"))), Ids("w-1"))
  }

object EdgeCaseTypesTest:
  object opaqueIds:
    opaque type Ids = String
    object Ids:
      def apply(s: String): Ids = s
  export opaqueIds.Ids

  case class WithOpaque(id: Ids, name: String)

  case class WithUnion(v: Int | String)

  trait MarkA
  trait MarkB
  case class WithIntersection(v: MarkA & MarkB)

  enum Expr[T]:
    case IntLit(v: Int) extends Expr[Int]
    case StrLit(v: String) extends Expr[String]

  @transparent
  case class OpaqueWrapper(inner: Ids)
