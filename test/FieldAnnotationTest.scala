package made

import made.annotation.*

class FieldAnnotationTest extends munit.FunSuite:

  // --- Field-level annotation queries ---

  test("hasAnnotation on annotated field") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: z *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Marker])
    assert(!y.hasAnnotation[Marker])
    assert(z.hasAnnotation[Marker])
  }

  test("getAnnotation on annotated field returns instance") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: _ *: _ *: EmptyTuple = mirror.elems
    assert(x.getAnnotation[Marker].isDefined)
  }

  test("getAnnotation returns None on unannotated field") {
    val mirror = Made.derived[AnnotatedFields]
    val _ *: y *: _ *: EmptyTuple = mirror.elems
    assert(y.getAnnotation[Marker].isEmpty)
  }

  test("parametrized annotation on field") {
    val mirror = Made.derived[FieldWithParamAnnotation]
    val x *: y *: EmptyTuple = mirror.elems
    assertEquals(x.getAnnotation[Tag].get.value, "primary")
    assertEquals(y.getAnnotation[Tag].get.value, "secondary")
  }

  test("multiple annotations on a single field") {
    val mirror = Made.derived[MultiAnnotatedField]
    val x *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Marker])
    assert(x.hasAnnotation[Tag])
    assertEquals(x.getAnnotation[Tag].get.value, "tagged")
  }

  test("field annotation does not bleed to type-level metadata") {
    val mirror = Made.derived[AnnotatedFields]
    assert(!mirror.hasAnnotation[Marker])
  }

  test("type-level annotation with field-level annotation are independent") {
    val mirror = Made.derived[BothLevelsAnnotated]
    assert(mirror.hasAnnotation[Tag])
    assertEquals(mirror.getAnnotation[Tag].get.value, "type-level")
    val x *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Tag])
    assertEquals(x.getAnnotation[Tag].get.value, "field-level")
  }

  // --- Annotations on sum subtypes ---

  test("annotations on enum cases") {
    val mirror = Made.derived[AnnotatedEnum]
    val a *: b *: EmptyTuple = mirror.elems
    assert(a.hasAnnotation[Tag])
    assertEquals(a.getAnnotation[Tag].get.value, "first")
    assert(!b.hasAnnotation[Tag])
  }

  test("annotations on sealed trait subtypes") {
    val mirror = Made.derived[AnnotatedADT]
    val x *: y *: EmptyTuple = mirror.elems
    assert(x.hasAnnotation[Marker])
    assert(!y.hasAnnotation[Marker])
  }

  // --- @name combined with other annotations on fields ---

  test("@name and custom annotation on same field") {
    val mirror = Made.derived[NameAndAnnotation]
    val x *: EmptyTuple = mirror.elems
    assertEquals(x.label, "renamed")
    assert(x.hasAnnotation[Marker])
  }

  // --- Annotation on @generated member ---

  test("field annotation on generated member") {
    val mirror = Made.derived[GeneratedWithFieldAnnotation]
    val gen *: EmptyTuple = mirror.generatedElems
    assert(gen.hasAnnotation[Tag])
    assertEquals(gen.getAnnotation[Tag].get.value, "computed")
    assert(gen.hasAnnotation[generated])
  }

  // --- Compile-time singleton narrowing ---

  test("hasAnnotation narrows to singleton true/false") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems
    val b1: true = x.hasAnnotation[Marker]
    val b2: false = y.hasAnnotation[Marker]
    assert(b1)
    assert(!b2)
  }

  test("getAnnotation narrows to singleton Some/None") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems
    val s: Some[Marker] = x.getAnnotation[Marker]
    val n: None.type = y.getAnnotation[Marker]
    assert(s.value.isInstanceOf[Marker])
    assertEquals(n, None)
  }

  test("inline if on hasAnnotation specialises at compile time") {
    val mirror = Made.derived[AnnotatedFields]
    val x *: y *: _ *: EmptyTuple = mirror.elems
    inline def tag(has: Boolean): String =
      inline if has then "yes" else "no"
    assertEquals(tag(x.hasAnnotation[Marker]), "yes")
    assertEquals(tag(y.hasAnnotation[Marker]), "no")
  }

  // --- Plural tuple-of-elements queries ---

  test("hasAnnotations returns tuple of singleton booleans") {
    val mirror = Made.derived[AnnotatedFields]
    val flags: (true, false, true) = mirror.elems.hasAnnotations[Marker]
    assertEquals(flags, (true, false, true))
  }

  test("getAnnotations returns tuple of singleton Some/None") {
    val mirror = Made.derived[AnnotatedFields]
    val opts: (Some[Marker], None.type, Some[Marker]) = mirror.elems.getAnnotations[Marker]
    assert(opts._1.value.isInstanceOf[Marker])
    assertEquals(opts._2, None)
    assert(opts._3.value.isInstanceOf[Marker])
  }

  test("hasAnnotations / getAnnotations narrow per-element") {
    val mirror = Made.derived[FieldWithParamAnnotation]
    val flags: (true, true) = mirror.elems.hasAnnotations[Tag]
    val opts: (Some[Tag], Some[Tag]) = mirror.elems.getAnnotations[Tag]
    assertEquals(flags, (true, true))
    assertEquals(opts._1.value.value, "primary")
    assertEquals(opts._2.value.value, "secondary")
  }

  test("hasAnnotations on EmptyTuple") {
    val flags: EmptyTuple = EmptyTuple.hasAnnotations[Marker]
    assertEquals(flags, EmptyTuple)
  }

// --- Fixtures ---

class Marker extends MetaAnnotation
case class Tag(value: String) extends MetaAnnotation

case class AnnotatedFields(@Marker x: Int, y: String, @Marker z: Boolean)
case class FieldWithParamAnnotation(@Tag("primary") x: String, @Tag("secondary") y: String)
case class MultiAnnotatedField(@Marker @Tag("tagged") x: Int)

@Tag("type-level")
case class BothLevelsAnnotated(@Tag("field-level") x: Int)

enum AnnotatedEnum:
  @Tag("first") case A
  case B

sealed trait AnnotatedADT
object AnnotatedADT:
  @Marker case class X(v: Int) extends AnnotatedADT
  case class Y(v: String) extends AnnotatedADT

case class NameAndAnnotation(@name("renamed") @Marker field: Int)

case class GeneratedWithFieldAnnotation(x: Int):
  @generated @Tag("computed") def computed: String = x.toString
