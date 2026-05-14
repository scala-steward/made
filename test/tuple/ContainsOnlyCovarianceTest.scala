package made

class ContainsOnlyCovarianceTest extends munit.FunSuite:
  sealed trait Fruit
  case class Apple() extends Fruit
  case class Banana() extends Fruit

  test("containsOnly is contravariant in Tup") {
    summon[(Fruit, Fruit) containsOnly Fruit]
    // Since (Apple, Apple) <: (Fruit, Fruit)
    // and containsOnly is -Tup
    // then containsOnly[(Fruit, Fruit), Fruit] <: containsOnly[(Apple, Apple), Fruit]
    summon[(Apple, Apple) containsOnly Fruit]
  }
