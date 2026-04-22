package made

import made.annotation.*

class DoneTest extends munit.FunSuite:

  test("trait with multiple methods, annotations, and mixed arities") {
    val _: Done {
      type Type = NamedService
      type Label = "NamedService"
      type Metadata = Meta @ServiceMarker
      type Operations = DoneOperation {
        type Label = "external_id"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = NamedService
        type OutputType = String
      } *: DoneOperation {
        type Label = "ping"
        type Metadata = Meta @MethodMarker
        type InputElems =
          InputElem {
            type Type = String
            type Label = "message"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = NamedService
        type OutputType = Boolean
      } *: DoneOperation {
        type Label = "combine"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "left"
            type Metadata = Meta
          } *: InputElem {
            type Type = Int
            type Label = "right"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = NamedService
        type OutputType = String
      } *: DoneOperation {
        type Label = "version"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = NamedService
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[NamedService]
  }

  test("class with inherited @name on overridden method") {
    val _: Done {
      type Type = AliasImpl
      type Label = "AliasImpl"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "endpoint"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = AliasImpl
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[AliasImpl]
  }

  test("abstract class with abstract and concrete methods") {
    val _: Done {
      type Type = AbstractJob
      type Label = "AbstractJob"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "run"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "task"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = AbstractJob
        type OutputType = Either[String, Int]
      } *: DoneOperation {
        type Label = "cached"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = AbstractJob
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[AbstractJob]
  }

  test("generic trait specializes output type") {
    val _: Done {
      type Type = Converter[Int]
      type Label = "Converter"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "convert"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "raw"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = Converter[Int]
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[Converter[Int]]
  }

  test("singleton object with multiple methods") {
    val _: Done {
      type Type = Toolbox.type
      type Label = "Toolbox"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "hello"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = Toolbox.type
        type OutputType = String
      } *: DoneOperation {
        type Label = "concat"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "left"
            type Metadata = Meta
          } *: InputElem {
            type Type = String
            type Label = "right"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = Toolbox.type
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[Toolbox.type]
  }

  test("enum with methods declared on the enum itself") {
    val _: Done {
      type Type = ServiceState
      type Label = "ServiceState"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "code"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = ServiceState
        type OutputType = String
      } *: DoneOperation {
        type Label = "isTerminal"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = ServiceState
        type OutputType = Boolean
      } *: EmptyTuple
    } = Done.derived[ServiceState]
  }

  test("trait with @name on the type and a method") {
    val _: Done {
      type Type = RenamedService
      type Label = "custom_service"
      type Metadata <: Meta
      type Operations = DoneOperation {
        type Label = "id"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = RenamedService
        type OutputType = String
      } *: DoneOperation {
        type Label = "ping"
        type Metadata = Meta @MethodMarker
        type InputElems = EmptyTuple
        type OuterType = RenamedService
        type OutputType = Boolean
      } *: EmptyTuple
    } = Done.derived[RenamedService]
  }

  test("methods returning complex types are preserved") {
    val _: Done {
      type Type = ComplexReturns
      type Label = "ComplexReturns"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "items"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = ComplexReturns
        type OutputType = List[String]
      } *: DoneOperation {
        type Label = "mapping"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = ComplexReturns
        type OutputType = Map[String, Int]
      } *: DoneOperation {
        type Label = "either"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "name"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = ComplexReturns
        type OutputType = Either[String, Int]
      } *: EmptyTuple
    } = Done.derived[ComplexReturns]
  }

  test("trait without methods has empty operations") {
    val _: Done {
      type Type = EmptyService
      type Label = "EmptyService"
      type Metadata = Meta
      type Operations = EmptyTuple
    } = Done.derived[EmptyService]
  }

  test("methods with multiple parameter lists are represented as one operation") {
    val _: Done {
      type Type = MultiListService
      type Label = "MultiListService"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "combine"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "left"
            type Metadata = Meta
          } *: InputElem {
            type Type = Int
            type Label = "right"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = MultiListService
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[MultiListService]
  }

  test("object methods with different return types") {
    val _: Done {
      type Type = MathTools.type
      type Label = "MathTools"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "zero"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = MathTools.type
        type OutputType = Int
      } *: DoneOperation {
        type Label = "toText"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = Int
            type Label = "value"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = MathTools.type
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[MathTools.type]
  }

  test("abstract val declarations are treated as nullary operations") {
    val _: Done {
      type Type = ValHolder
      type Label = "ValHolder"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "message"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = ValHolder
        type OutputType = String
      } *: DoneOperation {
        type Label = "magic"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = ValHolder
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[ValHolder]
  }

  test("Unit return type is preserved") {
    val _: Done {
      type Type = UnitReturning
      type Label = "UnitReturning"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "log"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "msg"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = UnitReturning
        type OutputType = Unit
      } *: DoneOperation {
        type Label = "tick"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = UnitReturning
        type OutputType = Unit
      } *: EmptyTuple
    } = Done.derived[UnitReturning]
  }

  test("function and tuple return types are preserved") {
    val _: Done {
      type Type = FuncAndTuple
      type Label = "FuncAndTuple"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "mkAdder"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = Int
            type Label = "base"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = FuncAndTuple
        type OutputType = Int => Int
      } *: DoneOperation {
        type Label = "pair"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = FuncAndTuple
        type OutputType = (Int, String)
      } *: EmptyTuple
    } = Done.derived[FuncAndTuple]
  }

  test("methods with empty parameter list yield empty InputElems") {
    val _: Done {
      type Type = EmptyParens
      type Label = "EmptyParens"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "a"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = EmptyParens
        type OutputType = Int
      } *: DoneOperation {
        type Label = "b"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = EmptyParens
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[EmptyParens]
  }

  test("methods inherited without override do not appear in child operations") {
    val _: Done {
      type Type = NonOverridingChild
      type Label = "NonOverridingChild"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "childOnly"
        type Metadata = Meta
        type InputElems = EmptyTuple
        type OuterType = NonOverridingChild
        type OutputType = Int
      } *: EmptyTuple
    } = Done.derived[NonOverridingChild]
  }

  test("apply invokes parameterless method on target instance") {
    val done = Done.derived[Calculator]
    val _ *: _ *: answerOp *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(answerOp.apply(impl, EmptyTuple), 42)
  }

  test("apply invokes method with single parameter list") {
    val done = Done.derived[Calculator]
    val addOp *: _ *: _ *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(addOp.apply(impl, (7, 5)), 12)
  }

  test("apply invokes method with multiple parameter lists") {
    val done = Done.derived[Calculator]
    val _ *: mulOp *: _ *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(mulOp.apply(impl, (6, 7)), 42)
  }

  test("Done.invoke fixes target to Done.Type for type safety") {
    val done = Done.derived[Calculator]
    val addOp *: _ *: _ *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = a + b
      def mul(a: Int)(b: Int): Int = a * b
      def answer: Int = 42

    assertEquals(done.invoke(addOp, impl, (2, 3)), 5)
  }

  test("apply dispatches virtually through an abstract member") {
    val done = Done.derived[Calculator]
    val _ *: _ *: answerOp *: EmptyTuple = done.operations

    val impl: Calculator = new Calculator:
      def add(a: Int, b: Int): Int = 0
      def mul(a: Int)(b: Int): Int = 0
      override def answer: Int = 99

    assertEquals(answerOp.apply(impl, EmptyTuple), 99)
  }

  test("apply invokes methods on a singleton object") {
    val done = Done.derived[MathTools.type]
    val zeroOp *: toTextOp *: EmptyTuple = done.operations

    assertEquals(zeroOp.apply(MathTools, EmptyTuple), 0)
    assertEquals(toTextOp.apply(MathTools, Tuple(123)), "123")
  }

  test("overloaded methods both appear as separate operations with the same label") {
    val _: Done {
      type Type = OverloadedCompute
      type Label = "OverloadedCompute"
      type Metadata = Meta
      type Operations = DoneOperation {
        type Label = "compute"
        type Metadata = Meta
        type InputElems = InputElem {
          type Type = Int
          type Label = "x"
          type Metadata = Meta
        } *: EmptyTuple
        type OuterType = OverloadedCompute
        type OutputType = Int
      } *: DoneOperation {
        type Label = "compute"
        type Metadata = Meta
        type InputElems =
          InputElem {
            type Type = String
            type Label = "x"
            type Metadata = Meta
          } *: EmptyTuple
        type OuterType = OverloadedCompute
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[OverloadedCompute]
  }

// --- Fixtures ---

class ServiceMarker extends MetaAnnotation
class MethodMarker extends MetaAnnotation

@ServiceMarker
trait NamedService:
  @name("external_id")
  def id: String

  @MethodMarker
  def ping(message: String): Boolean

  def combine(left: String)(right: Int): String

  def version: Int

trait AliasBase:
  @name("endpoint")
  def id: String

class AliasImpl extends AliasBase:
  override def id: String = "impl"

abstract class AbstractJob:
  def run(task: String): Either[String, Int]
  def cached: Int = 42

trait Converter[A]:
  def convert(raw: String): A

object Toolbox:
  def hello: String = "hi"
  def concat(left: String, right: String): String = left + right

enum ServiceState:
  case Open, Closed

  def code: String = productPrefix

  def isTerminal: Boolean = this match
    case Open => false
    case Closed => true

@name("custom_service")
trait RenamedService:
  def id: String

  @MethodMarker
  def ping: Boolean

trait ComplexReturns:
  def items: List[String]
  def mapping: Map[String, Int]
  def either(name: String): Either[String, Int]

trait EmptyService

trait MultiListService:
  def combine(left: String)(right: Int): String

object MathTools:
  def zero: Int = 0
  def toText(value: Int): String = value.toString

trait ValHolder:
  val magic: Int
  val message: String

trait UnitReturning:
  def log(msg: String): Unit
  def tick: Unit

trait FuncAndTuple:
  def mkAdder(base: Int): Int => Int
  def pair: (Int, String)

trait EmptyParens:
  def a: Int
  def b(): Int

trait NonOverridingParent:
  def inherited: String = "base"

class NonOverridingChild extends NonOverridingParent:
  def childOnly: Int = 42

trait OverloadedCompute:
  def compute(x: Int): Int
  def compute(x: String): String

trait Calculator:
  def add(a: Int, b: Int): Int
  def mul(a: Int)(b: Int): Int
  def answer: Int
