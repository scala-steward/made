package made

import made.containsOnly.given

import scala.language.implicitConversions

class ContainsOnlyTest extends munit.FunSuite:

  // --- Loop type-level: positive cases ---

  test("Loop: EmptyTuple containsOnly Int") {
    summon[containsOnly.Loop[EmptyTuple, Int] =:= true]
  }

  test("Loop: EmptyTuple containsOnly String") {
    summon[containsOnly.Loop[EmptyTuple, String] =:= true]
  }

  test("Loop: single Int containsOnly Int") {
    summon[containsOnly.Loop[Tuple1[Int], Int] =:= true]
  }

  test("Loop: single String containsOnly String") {
    summon[containsOnly.Loop[Tuple1[String], String] =:= true]
  }

  test("Loop: (Int, Int) containsOnly Int") {
    summon[containsOnly.Loop[(Int, Int), Int] =:= true]
  }

  test("Loop: (Int, Int, Int) containsOnly Int") {
    summon[containsOnly.Loop[(Int, Int, Int), Int] =:= true]
  }

  test("Loop: (String, String) containsOnly String") {
    summon[containsOnly.Loop[(String, String), String] =:= true]
  }

  test("Loop: (Boolean, Boolean, Boolean) containsOnly Boolean") {
    summon[containsOnly.Loop[(Boolean, Boolean, Boolean), Boolean] =:= true]
  }

  test("Loop: (Double, Double) containsOnly Double") {
    summon[containsOnly.Loop[(Double, Double), Double] =:= true]
  }

  // --- Loop type-level: negative cases ---

  test("Loop: (Int, String) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, String), Int] =:= false]
  }

  test("Loop: (String, Int) does not containOnly String") {
    summon[containsOnly.Loop[(String, Int), String] =:= false]
  }

  test("Loop: (Int, Int, String) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, Int, String), Int] =:= false]
  }

  test("Loop: (String, Int, String) does not containOnly String") {
    summon[containsOnly.Loop[(String, Int, String), String] =:= false]
  }

  test("Loop: (Int, Double) does not containOnly Int") {
    summon[containsOnly.Loop[(Int, Double), Int] =:= false]
  }

  test("Loop: single Int does not containOnly String") {
    summon[containsOnly.Loop[Tuple1[Int], String] =:= false]
  }

  // --- Loop type-level: subtype behavior ---

  test("Loop: subtypes with sealed trait") {
    sealed trait Fruit
    case class Apple() extends Fruit
    case class Banana() extends Fruit

    summon[containsOnly.Loop[(Apple, Banana), Fruit] =:= true]
  }

  test("Loop: single subtype with sealed trait") {
    sealed trait Fruit
    case class Apple() extends Fruit

    summon[containsOnly.Loop[Tuple1[Apple], Fruit] =:= true]
  }

  test("Loop: mixed subtype and unrelated type") {
    sealed trait Fruit
    case class Apple() extends Fruit

    summon[containsOnly.Loop[(Apple, String), Fruit] =:= false]
  }

  // --- given instance: evidence is summoned correctly ---

  test("given: evidence available for EmptyTuple") {
    summon[EmptyTuple containsOnly Int]
  }

  test("given: evidence available for (Int, Int, Int)") {
    summon[(Int, Int, Int) containsOnly Int]
  }

  test("given: evidence available for (String, String)") {
    summon[(String, String) containsOnly String]
  }

  test("given: evidence available for single element") {
    summon[Tuple1[Int] containsOnly Int]
  }

  test("given: no evidence for heterogeneous tuple") {
    val errors = compileErrors("summon[(Int, String) containsOnly Int]")
    assert(errors.nonEmpty, "Should not find evidence for heterogeneous tuple")
  }

  test("given: no evidence for wrong type") {
    val errors = compileErrors("summon[(Int, Int) containsOnly String]")
    assert(errors.nonEmpty, "Should not find evidence for wrong type")
  }

  // --- ---

  test("produces valid evidence") {
    val evidence: (Int, Int) containsOnly Int = containsOnly.refl
    assert(evidence.asInstanceOf[Boolean])
  }

  test("for EmptyTuple") {
    val evidence: EmptyTuple containsOnly String = containsOnly.refl
    assert(evidence.asInstanceOf[Boolean])
  }

  // --- integration with mapOnly ---

  test("mapOnly compiles for homogeneous Int tuple") {
    val result = (1, 2, 3).mapOnly[Int]([t <: Int] => (x: t) => List(x))
    assertEquals(result, (List(1), List(2), List(3)))
  }

  test("mapOnly compiles for homogeneous String tuple") {
    val result = ("a", "b").mapOnly[String]([t <: String] => (x: t) => Option(x))
    assertEquals(result, (Some("a"), Some("b")))
  }

  test("mapOnly compiles for empty tuple") {
    val result = EmptyTuple.mapOnly[Int]([t <: Int] => (x: t) => List(x))
    assertEquals(result, EmptyTuple)
  }

  test("mapOnly compiles for single element") {
    val result = Tuple1(42).mapOnly[Int]([t <: Int] => (x: t) => Option(x))
    assertEquals(result, Tuple1(Some(42)))
  }

  test("mapOnly compiles for subtype hierarchy") {
    sealed trait Animal
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal

    val result = (Dog("Rex"), Cat("Whiskers")).mapOnly[Animal]([t <: Animal] => (x: t) => Option(x))
    assertEquals(result, (Some(Dog("Rex")), Some(Cat("Whiskers"))))
  }

  // --- does not compile cases ---

  test("does not compile: heterogeneous tuple with mapOnly") {
    val errors = compileErrors("""
      (1, "a").mapOnly[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: wrong target type with mapOnly") {
    val errors = compileErrors("""
      (1, 2, 3).mapOnly[String]([t <: String] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: Int and Double mix") {
    val errors = compileErrors("""
      (1, 2.0).mapOnly[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: Boolean among Ints") {
    val errors = compileErrors("""
      (1, true, 2).mapOnly[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: String among Ints at end") {
    val errors = compileErrors("""
      (1, 2, "three").mapOnly[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("does not compile: String among Ints at start") {
    val errors = compileErrors("""
      ("zero", 1, 2).mapOnly[Int]([t <: Int] => (x: t) => List(x))
    """)
    assert(errors.nonEmpty)
  }

  test("evidence allows head access") {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    val first: Int = tuple.head
    assertEquals(first, 1)
  }

//  test("evidence allows indexed access") {
//    val tuple: (Any, Any, Any) = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val second: Int = tuple(1)
//    assertEquals(second, 2)
//  }

  test("evidence allows last access") {
    val tuple: Tuple = (1, 2, 3)
    given tuple.type containsOnly Int = containsOnly.refl

    val last: Int = tuple.last
    assertEquals(last, 3)
  }
//
//  test("evidence allows drop") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val dropped: (Int, Int) = tuple.drop(1)
//    assertEquals(dropped.toList, List(2, 3))
//  }
//
//  test("evidence allows take") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val taken: (Int, Int) = tuple.take(2)
//    assertEquals(taken.toList, List(1, 2))
//  }
//
//  test("evidence allows tail") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val rest: (Int, Int) = tuple.tail
//    assertEquals(rest.toList, List(2, 3))
//  }
//
//  test("evidence allows init") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val front: (Int, Int) = tuple.init
//    assertEquals(front.toList, List(1, 2))
//  }
//
//  test("evidence allows reverse") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val reversed: (Int, Int, Int) = tuple.reverse
//    assertEquals(reversed.toList, List(3, 2, 1))
//  }
//
//  test("evidence allows mapOnly") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val result = tuple.mapOnly[Int]([t <: Int] => (x: t) => List(x))
//    assertEquals(result.toList, List(List(1), List(2), List(3)))
//  }

//  test("evidence allows toList") {
//    val tuple: Tuple = (1, 2, 3)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    assertEquals(tuple.toList, List(1, 2, 3))
//  }
//
//  test("evidence allows concat") {
//    val tuple: Tuple = (1, 2)
//    given tuple.type containsOnly Int = containsOnly.refl
//
//    val result: Tuple = tuple ++ (3, 4)
//    assertEquals(result.toList, List(1, 2, 3, 4))
//  }

  test("evidence on single element") {
    val tuple: Tuple = Tuple1(42)
    given tuple.type containsOnly Int = containsOnly.refl

    val first: Int = tuple.head
    assertEquals(first, 42)
  }

  test("evidence with String tuple") {
    val tuple: Tuple = ("a", "b", "c")
    given tuple.type containsOnly String = containsOnly.refl

    val first: String = tuple.head
    assertEquals(first, "a")
    assertEquals(tuple.size, 3)
  }
