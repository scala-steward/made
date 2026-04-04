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
