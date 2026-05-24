package made

import made.annotation.MetaAnnotation
import made.util.SnippetCompiler
import made.util.SnippetCompiler.containsMessage

class MoreEdgeCasesTest extends munit.FunSuite:
  import MoreEdgeCasesTest.*

  test("empty case class") {
    val m = Made.derived[Empty]
    assertEquals(compiletime.constValueTuple[m.ElemLabels].toList, Nil)
    assertEquals(m.fromTuple(EmptyTuple), Empty())
  }

  test("enum with a single case") {
    val m = Made.derived[Single]
    assertEquals(m.ordinal(Single.Only), 0)
  }

  test("recursive product via List<Self>") {
    val m = Made.derived[Node]
    val v *: kids *: EmptyTuple = m.elems
    assertEquals(v.label, "value")
    assertEquals(kids.label, "children")
    val tree = Node(1, List(Node(2, Nil), Node(3, Nil)))
    assertEquals(v.apply(tree), 1)
    assertEquals(kids.apply(tree).size, 2)
  }

  test("path-dependent field type via a fixed Outer") {
    val m = Made.derived[Holds]
    val inner *: EmptyTuple = m.elems
    assertEquals(inner.apply(Holds(Outer.shared.Inner("x"))).name, "x")
  }

  test("case object extending two sealed parents shows up in both") {
    val mA = Made.derived[SubA]
    val mB = Made.derived[SubB]
    assertEquals(mA.ordinal(Multi), 0)
    assertEquals(mB.ordinal(Multi), 0)
  }

  test("annotation declared on parent trait is read on the case class field") {
    val m = Made.derived[WithInherited]
    val x *: EmptyTuple = m.elems
    assert(x.hasAnnotation[JsonName])
    assertEquals(x.getAnnotation[JsonName].map(_.value), Some("inherited"))
  }

  test("enum cases sharing field names produce matching elem labels") {
    val m = Made.derived[Op]
    val (add, sub) = m.elems
    assertEquals(add.label, "Add")
    assertEquals(sub.label, "Sub")
  }

  test("Made.derived requires a concrete type — type constructor List is rejected") {
    val diags = SnippetCompiler.compile(
      // language=scala 3
      """import made.*
        |object S { val _ = Made.derived[List] }
        |""".stripMargin,
    )
    assert(
      diags.containsMessage("Unsupported Mirror type") || diags.containsMessage("does not conform"),
      s"expected derivation rejection but got: $diags",
    )
  }

  // `derives Made` is not supported: `Made` carries `type Type` instead of a type parameter,
  // so the standard `T derives Made` desugaring does not apply. Users summon via `Made.Of[T]`
  // or `Made.derived[T]` directly.

object MoreEdgeCasesTest:
  case class Empty()

  enum Single:
    case Only

  case class Node(value: Int, children: List[Node])

  class Outer:
    case class Inner(name: String)
  object Outer:
    val shared: Outer = new Outer
  case class Holds(inner: Outer.shared.Inner)

  sealed trait SubA
  sealed trait SubB
  case object Multi extends SubA, SubB

  class JsonName(val value: String) extends MetaAnnotation

  trait HasJson:
    @JsonName("inherited")
    def x: Int

  case class WithInherited(x: Int) extends HasJson

  enum Op:
    case Add(lhs: Int, rhs: Int)
    case Sub(lhs: Int, rhs: Int)
