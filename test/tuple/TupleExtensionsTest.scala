package made.tuple

class TupleExtensionsTest extends munit.FunSuite:

  // --- foreach ---

  test("foreach on empty tuple does nothing") {
    var count = 0
    EmptyTuple.foreach([t] => (_: t) => count += 1)
    assertEquals(count, 0)
  }

  test("foreach visits every element") {
    var count = 0
    (1, 2, 3).foreach([t] => (_: t) => count += 1)
    assertEquals(count, 3)
  }

  test("foreach visits elements in order") {
    val builder = List.newBuilder[Any]
    ("a", "b", "c").foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List("a", "b", "c"))
  }

  test("foreach works on single element tuple") {
    val builder = List.newBuilder[Any]
    Tuple1(42).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(42))
  }

  test("foreach works on heterogeneous tuple") {
    val builder = List.newBuilder[Any]
    (1, "two", 3.0, true).foreach([t] => (x: t) => builder += x)
    assertEquals(builder.result(), List(1, "two", 3.0, true))
  }

  test("foreach can perform side effects") {
    var sum = 0
    (10, 20, 30).foreach([t] => (x: t) => sum += x.asInstanceOf[Int])
    assertEquals(sum, 60)
  }

  // --- indices ---

  test("indices of empty tuple") {
    val result = EmptyTuple.indices
    assertEquals(result, EmptyTuple)
  }

  test("indices of single element tuple") {
    val result = Tuple1("a").indices
    assertEquals(result, Tuple1(0))
  }

  test("indices of three element tuple") {
    val result = (10, 20, 30).indices
    assertEquals(result, (0, 1, 2))
  }

  test("indices of five element tuple") {
    val result = ("a", "b", "c", "d", "e").indices
    assertEquals(result, (0, 1, 2, 3, 4))
  }

  test("indices count matches tuple size") {
    val tuple = (1, 2, 3, 4)
    assertEquals(tuple.indices.size, tuple.size)
  }

  test("indices of heterogeneous tuple") {
    val result = (1, "two", 3.0).indices
    assertEquals(result, (0, 1, 2))
  }
