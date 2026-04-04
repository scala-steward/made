package made

class ToArrayOfTest extends munit.FunSuite:

  test("empty tuple returns empty array") {
    val result = EmptyTuple.toArrayOf[Int]
    assertEquals(result.toList, List.empty[Int])
  }

  test("single element tuple") {
    val tuple = Tuple1(42)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, List(42))
  }

  test("multiple elements") {
    val tuple = (1, 2, 3)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, List(1, 2, 3))
  }

  test("string elements") {
    val tuple = ("a", "b", "c")
    val result = tuple.toArrayOf[String]
    assertEquals(result.toList, List("a", "b", "c"))
  }

  test("preserves element order") {
    val tuple = (10, 20, 30, 40, 50)
    val result = tuple.toArrayOf[Int]
    assertEquals(result.toList, List(10, 20, 30, 40, 50))
  }

  test("result array has correct length") {
    val tuple = ("x", "y", "z")
    val result = tuple.toArrayOf[String]
    assertEquals(result.length, 3)
  }

  test("supertype target") {
    val tuple = (1, 2, 3)
    val result = tuple.toArrayOf[Any]
    assertEquals(result.toList, List(1, 2, 3))
  }
