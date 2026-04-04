package made.tuple

class ForeachTest extends munit.FunSuite:

  test("empty tuple does nothing") {
    var count = 0
    EmptyTuple.foreach([t] => (_: t) => count += 1)
    assertEquals(count, 0)
  }

  test("visits every element") {
    var count = 0
    (1, 2, 3).foreach([t] => (_: t) => count += 1)
    assertEquals(count, 3)
  }

  test("visits elements in order") {
    val builder = List.newBuilder[Any]
    ("a", "b", "c").foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List("a", "b", "c"))
  }

  test("single element tuple") {
    val builder = List.newBuilder[Any]
    Tuple1(42).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(42))
  }

  test("heterogeneous tuple") {
    val builder = List.newBuilder[Any]
    (1, "two", 3.0, true).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(1, "two", 3.0, true))
  }

  test("side effects") {
    var sum = 0
    (10, 20, 30).foreach([t] => (x: t) => sum += x.asInstanceOf[Int])
    assertEquals(sum, 60)
  }
