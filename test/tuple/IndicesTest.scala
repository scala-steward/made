package made.tuple

class IndicesTest extends munit.FunSuite:

  test("empty tuple") {
    val result = EmptyTuple.indices
    assertEquals(result, EmptyTuple)
  }

  test("single element tuple") {
    val result = Tuple1("a").indices
    assertEquals(result, Tuple1(0))
  }

  test("three element tuple") {
    val result = (10, 20, 30).indices
    assertEquals(result, (0, 1, 2))
  }

  test("five element tuple") {
    val result = ("a", "b", "c", "d", "e").indices
    assertEquals(result, (0, 1, 2, 3, 4))
  }

  test("size matches tuple size") {
    val tuple = (1, 2, 3, 4)
    assertEquals(tuple.indices.size, tuple.size)
  }

  test("heterogeneous tuple") {
    val result = (1, "two", 3.0).indices
    assertEquals(result, (0, 1, 2))
  }
