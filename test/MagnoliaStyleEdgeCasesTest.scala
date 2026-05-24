package made

import made.util.SnippetCompiler
import made.util.SnippetCompiler.containsMessage

class MagnoliaStyleEdgeCasesTest extends munit.FunSuite:
  import MagnoliaStyleEdgeCasesTest.*

  test("derives a case class with > 22 fields") {
    val m = Made.derived[Big]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList.size, 25)
    val instance = Big(
      1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25,
    )
    val a *: _ = m.elems
    assertEquals(a.apply(instance), 1)
  }

  test("derives a mutually recursive ADT") {
    val mA = Made.derived[NodeA]
    val mB = Made.derived[NodeB]
    assertEquals(compiletime.constValueTuple[mA.ElemLabels].toList, List("value", "next"))
    assertEquals(compiletime.constValueTuple[mB.ElemLabels].toList, List("label", "child"))
  }

  test("derives a covariant generic ADT") {
    val m = Made.derived[Box[String]]
    val v *: EmptyTuple = m.elems
    assertEquals(v.label, "value")
    assertEquals(v.apply(Box("x")), "x")
  }

  test("derives a phantom-tagged wrapper") {
    val m = Made.derived[Tagged[String, UserTag]]
    val v *: EmptyTuple = m.elems
    assertEquals(v.apply(Tagged[String, UserTag]("u-1")), "u-1")
  }

  test("multiple parameter lists are rejected by the compiler-provided Mirror") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import made.*
        |case class Multi(a: Int)(b: String)
        |object S { val _ = Made.derived[Multi] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type") || diags.containsMessage("No given instance"),
      s"expected unsupported diagnostic but got: $diags",
    )
  }

  test("private-constructor case class derives within its scope") {
    val m = PrivateScope.derive
    val x *: EmptyTuple = m.elems
    assertEquals(x.apply(PrivateScope.make(7)), 7)
  }

  test("recursive ADT (linked list)") {
    val m = Made.derived[LList[Int]]
    assertEquals(m.ordinal(LList.Nil), 1)
    assertEquals(m.ordinal(LList.Cons(1, LList.Nil)), 0)
  }

object MagnoliaStyleEdgeCasesTest:
  case class Big(
    a01: Int,
    a02: Int,
    a03: Int,
    a04: Int,
    a05: Int,
    a06: Int,
    a07: Int,
    a08: Int,
    a09: Int,
    a10: Int,
    a11: Int,
    a12: Int,
    a13: Int,
    a14: Int,
    a15: Int,
    a16: Int,
    a17: Int,
    a18: Int,
    a19: Int,
    a20: Int,
    a21: Int,
    a22: Int,
    a23: Int,
    a24: Int,
    a25: Int,
  )

  case class NodeA(value: Int, next: Option[NodeB])
  case class NodeB(label: String, child: Option[NodeA])

  case class Box[+A](value: A)

  trait UserTag
  case class Tagged[A, Tag](value: A)

  object PrivateScope:
    case class Hidden private[PrivateScope] (x: Int)
    def make(x: Int): Hidden = Hidden(x)
    def derive = Made.derived[Hidden]

  enum LList[+A]:
    case Cons(head: A, tail: LList[A])
    case Nil extends LList[Nothing]
