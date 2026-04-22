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
        type OutputType = String
      } *: DoneOperation {
        type Label = "version"
        type Metadata = Meta
        type InputElems = EmptyTuple
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
        type OutputType = Either[String, Int]
      } *: DoneOperation {
        type Label = "cached"
        type Metadata = Meta
        type InputElems = EmptyTuple
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
        type OutputType = String
      } *: DoneOperation {
        type Label = "isTerminal"
        type Metadata = Meta
        type InputElems = EmptyTuple
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
        type OutputType = String
      } *: DoneOperation {
        type Label = "ping"
        type Metadata = Meta @MethodMarker
        type InputElems = EmptyTuple
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
        type OutputType = List[String]
      } *: DoneOperation {
        type Label = "mapping"
        type Metadata = Meta
        type InputElems = EmptyTuple
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
        type OutputType = String
      } *: EmptyTuple
    } = Done.derived[MathTools.type]
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
