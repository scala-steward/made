package made

import made.annotation.MetaAnnotation

class ExtractMetaTest extends munit.FunSuite:

  class Marker extends MetaAnnotation
  case class T(@Marker x: Int)

  test("ExtractMeta on MadeFieldElem subtype refinement") {
    val m = Made.derived[T]
    val head *: _ = m.elems
    summon[MadeElem.ExtractMeta[head.type] =:= ((Meta @Marker) *: EmptyTuple)]
  }
