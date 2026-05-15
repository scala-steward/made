package made

import made.annotation.*

class PathDependentEvidenceTest extends munit.FunSuite:
  import PathDependentEvidenceTest.*

  // --- Made: path-dependent givens ---

  test("Made: Elems containsOnly MadeElem summons without import") {
    val m = Made.derived[PProduct]
    summon[m.Elems containsOnly MadeElem]
  }

  test("Made: Metadata containsOnly Meta summons without import") {
    val m = Made.derived[PAnnotated]
    summon[m.Metadata containsOnly Meta]
  }

  test("Made: GeneratedElems containsOnly GeneratedMadeElem summons without import") {
    val m = Made.derived[PWithGenerated]
    summon[m.GeneratedElems containsOnly GeneratedMadeElem]
  }

  test("Made: GeneratedElems containsOnly MadeElem via contravariance") {
    val m = Made.derived[PWithGenerated]
    summon[m.GeneratedElems containsOnly MadeElem]
  }

  test("Made: generatedElems.hasAnnotations works") {
    val m = Made.derived[PWithGenerated]
    val flags: (true, true) = m.generatedElems.hasAnnotations[generated]
    assertEquals(flags, (true, true))
  }

  test("Made: generatedElems.getAnnotations works") {
    val m = Made.derived[PWithGenerated]
    val opts: (Some[generated], Some[generated]) = m.generatedElems.getAnnotations[generated]
    assert(opts._1.value.isInstanceOf[generated])
    assert(opts._2.value.isInstanceOf[generated])
  }

  test("MadeElem: Metadata containsOnly Meta summons without import") {
    val m = Made.derived[PAnnotated]
    val x *: _ = m.elems
    summon[x.Metadata containsOnly Meta]
  }

  // --- Done: path-dependent givens ---

  test("Done: Metadata containsOnly Meta summons without import") {
    val d = Done.derived[PSvc]
    summon[d.Metadata containsOnly Meta]
  }

  test("Done: Operations containsOnly DoneOperation summons without import") {
    val d = Done.derived[PSvc]
    summon[d.Operations containsOnly DoneOperation]
  }

  test("DoneOperation: Metadata containsOnly Meta summons without import") {
    val d = Done.derived[PSvc]
    val op *: _ = d.operations
    summon[op.Metadata containsOnly Meta]
  }

  test("DoneOperation: InputElems containsOnly InputElem summons without import") {
    val d = Done.derived[PSvc]
    val op *: _ = d.operations
    summon[op.InputElems containsOnly InputElem]
  }

  test("InputElem: Metadata containsOnly Meta summons without import") {
    val d = Done.derived[PSvc]
    val _ *: op2 *: EmptyTuple = d.operations
    val ie *: _ = op2.inputElems
    summon[ie.Metadata containsOnly Meta]
  }

object PathDependentEvidenceTest:
  class PMarker extends MetaAnnotation

  case class PProduct(a: Int, b: String)

  @PMarker
  case class PAnnotated(@PMarker x: Int, y: String)

  case class PWithGenerated(s: String):
    @generated def len: Int = s.length
    @generated def upper: String = s.toUpperCase

  @PMarker
  trait PSvc:
    @PMarker def op1: Int
    def op2(x: String): Boolean
