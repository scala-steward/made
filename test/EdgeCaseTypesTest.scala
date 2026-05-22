package made

import made.annotation.transparent
import made.util.SnippetCompiler
import made.util.SnippetCompiler.containsMessage

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
    val readInt: Int | String = v.apply(WithUnion(42))
    val readStr: Int | String = v.apply(WithUnion("x"))
    assertEquals(readInt, 42: Int | String)
    assertEquals(readStr, "x": Int | String)
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

  // TODO: named tuple — deriver succeeds for (name: String, age: Int) but exposes empty ElemLabels.
  //   Re-enable when Mirror.ProductOf handling of named tuples is investigated:
  //
  //   test("derives Product for named tuple") {
  //     val m = Made.derived[PersonNT]
  //     assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("name", "age"))
  //   }

  test("derives Product for plain tuple") {
    val m = Made.derived[(Int, String)]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("_1", "_2"))
    val v: (Int, String) = m.fromTuple((1, "x"))
    assertEquals(v, (1, "x"))
  }

  // TODO: type alias — deriver succeeds for `type AliasFoo = Foo` but exposes empty ElemLabels.
  //   Probably the macro looks at the alias rather than dealiased Foo:
  //
  //   test("derives the underlying mirror through a type alias") {
  //     val m = Made.derived[AliasFoo]
  //     assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("x"))
  //   }

  test("derives the underlying Product for a refinement of a case class") {
    val m = Made.derived[Foo { val x: Int }]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, List("x"))
  }

  test("union type as mirrored type is rejected") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import made.*
        |object S { val _ = Made.derived[Int | String] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type"),
      s"expected 'Unsupported Mirror type' diagnostic but got: $diags",
    )
  }

  test("non-sealed trait without a Mirror is rejected") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import made.*
        |trait Open
        |object S { val _ = Made.derived[Open] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type"),
      s"expected 'Unsupported Mirror type' diagnostic but got: $diags",
    )
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

  type PersonNT = (name: String, age: Int)

  case class Foo(x: Int)
  type AliasFoo = Foo
