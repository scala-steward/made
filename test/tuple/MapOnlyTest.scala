package made.tuple

class MapOnlyTest extends munit.FunSuite:

  test("mapOnly on empty tuple") {
    val result = EmptyTuple.mapOnly[Int, Tuple1]([t <: Int] => (x: t) => Tuple1(x))
    assertEquals(result, EmptyTuple)
  }

  test("mapOnly transforms all elements of homogeneous tuple") {
    val result = (1, 2, 3).mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(1), List(2), List(3)))
  }

  test("mapOnly with Option wrapper") {
    val result = ("a", "b").mapOnly[String, Option]([t <: String] => (x: t) => Option(x))
    assertEquals(result, (Some("a"), Some("b")))
  }

  test("mapOnly with single element tuple") {
    val result = Tuple1(42).mapOnly[Int, Some]([t <: Int] => (x: t) => Some(x))
    assertEquals(result, Tuple1(Some(42)))
  }

  test("mapOnly with identity-like wrapper") {
    type Id[X <: Int] = X
    val result = (1, 2, 3).mapOnly[Int, Id]([t <: Int] => (x: t) => x: Id[t])
    assertEquals(result, (1, 2, 3))
  }

  test("mapOnly with Set wrapper") {
    val result = (1, 2, 3).mapOnly[Int, Set]([t <: Int] => (x: t) => Set(x))
    assertEquals(result, (Set(1), Set(2), Set(3)))
  }

  test("mapOnly preserves element order") {
    val result = (10, 20, 30, 40).mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(10), List(20), List(30), List(40)))
  }

  test("mapOnly with subtype constraint") {
    sealed trait Animal
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val result = (Dog("Rex"), Cat("Whiskers")).mapOnly[Animal, Option]([t <: Animal] => (x: t) => Option(x))
    assertEquals(result, (Some(Dog("Rex")), Some(Cat("Whiskers"))))
  }

  test("mapOnly for tail") {
    val result = (10, 20, 30, 40).tail.mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(20), List(30), List(40)))
  }

  test("mapOnly for drop") {
    val result = (1, 2, 3, 4, 5).drop(2).mapOnly[Int, Option]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, (Some(3), Some(4), Some(5)))
  }

  test("mapOnly for take") {
    val result = (1, 2, 3, 4, 5).take(3).mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(1), List(2), List(3)))
  }

  test("mapOnly for init") {
    val result = (1, 2, 3).init.mapOnly[Int, Option]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, (Some(1), Some(2)))
  }

  test("mapOnly for reverse") {
    val result = (1, 2, 3).reverse.mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(3), List(2), List(1)))
  }

  test("mapOnly for concat") {
    val result = ((1, 2) ++ (3, 4)).mapOnly[Int, Option]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, (Some(1), Some(2), Some(3), Some(4)))
  }

  test("mapOnly for splitAt first half") {
    val (first, _) = (1, 2, 3, 4).splitAt(2)
    val result = first.mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(1), List(2)))
  }

  test("mapOnly for chained tail") {
    val result = (1, 2, 3, 4, 5).tail.tail.mapOnly[Int, Option]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, (Some(3), Some(4), Some(5)))
  }

  test("mapOnly for tail to single element") {
    val result = (1, 2).tail.mapOnly[Int, List]([t <: Int] => (x: t) => List(x))
    assertEquals(result, Tuple1(List(2)))
  }

  test("mapOnly for drop then take") {
    val result = (1, 2, 3, 4, 5).drop(1).take(2).mapOnly[Int, Option]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, (Some(2), Some(3)))
  }
