package made.tuple

class ContainsOnlyTest extends munit.FunSuite:

  test("does not compile for heterogeneous tuple") {
    assert(compileErrors("import made.tuple.containsOnly.given; (1, \"a\").toArrayOf[Int]").nonEmpty)
  }

  test("does not compile for wrong target type") {
    assert(compileErrors("import made.tuple.containsOnly.given; (1, 2, 3).toArrayOf[String]").nonEmpty)
  }
