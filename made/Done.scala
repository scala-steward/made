package made

import scala.quoted.*

/**
 * Mirror for operation-centric types, describing the methods and fields of a type `T`
 * as a tuple of [[DoneOperation]]s.
 *
 * While [[Made]] models a type by its constructor parameters or subtypes, `Done` models
 * it by its members: each `val`, `def`, or field becomes an operation whose input
 * parameters and output type are captured at the type level. This makes `Done` suited to
 * describing services, RPC interfaces, enums with behavior, and other types where the API
 * surface (rather than the data shape) is what matters.
 *
 * @example
 * {{{
 * import made.*
 *
 * trait Service:
 *   def ping(message: String): Boolean
 *   def version: Int
 *
 * val done: Done.Of[Service] = Done.derived[Service]
 * // done type members:
 * //   type Type = Service
 * //   type Label = "Service"
 * //   type Metadata = Meta
 * //   type Operations = DoneOperation { ... "ping" ... } *: DoneOperation { ... "version" ... } *: EmptyTuple
 * }}}
 *
 * @see [[DoneOperation]]
 * @see [[InputElem]]
 * @see [[Done.derived]]
 */
sealed trait Done:
  /** The mirrored type `T`. */
  type Type

  /** The simple name of `T` (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on `T`, represented as an `AnnotatedType` chain wrapping the [[Meta]]
   * base type. When no `MetaAnnotation` annotations are present, `Metadata = Meta`. When
   * annotations are present, `Metadata` becomes `Meta @Ann1 @Ann2 ...`.
   */
  type Metadata <: Meta

  /** Tuple of [[DoneOperation]] subtypes, one per field or method of `T`. */
  type Operations <: Tuple

  def operations: Operations

/**
 * Element representing a single operation (field or method) in a [[Done]] mirror.
 *
 * Each entry in [[Done.Operations]] is a `DoneOperation` describing one member of the
 * mirrored type: its label, metadata, input parameters, and output type. Methods with
 * multiple parameter lists are flattened into a single [[InputElems]] tuple.
 *
 * @see [[Done]]
 * @see [[InputElem]]
 */
sealed trait DoneOperation:
  type Type

  /** The member name (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on the member, represented as an `AnnotatedType` chain wrapping
   * the [[Meta]] base type.
   */
  type Metadata <: Meta

  /** Tuple of [[InputElem]] subtypes describing the operation's parameters, in declaration order. */
  type InputElems <: Tuple

  def inputElems: InputElems

  /** The return type of the operation. */
  type OutputType

/**
 * Element representing a single input parameter of a [[DoneOperation]].
 *
 * Carries the parameter's type, label, and annotation metadata.
 *
 * @see [[DoneOperation]]
 * @see [[Done]]
 */
sealed trait InputElem:
  /** The parameter's type. */
  type Type

  /** The parameter's name (or the override provided by `@name`). */
  type Label <: String

  /**
   * Annotation metadata on the parameter, represented as an `AnnotatedType` chain wrapping
   * the [[Meta]] base type.
   */
  type Metadata <: Meta

object Done:
  type Of[T] = Done { type Type = T }

  /**
   * Derives a [[Done]] mirror for `T` at compile time.
   *
   * Each field member and declared method of `T` becomes a [[DoneOperation]] in the
   * resulting [[Done.Operations]] tuple. Methods with multiple parameter lists are
   * flattened into a single [[DoneOperation.InputElems]] tuple.
   *
   * @see [[Done]]
   * @see [[DoneOperation]]
   * @see [[InputElem]]
   */
  transparent inline given derived[T]: Done.Of[T] = ${ derivedImpl[T] }

  private def derivedImpl[T: Type](using quotes: Quotes): Expr[Done.Of[T]] =
    import quotes.reflect.*
    val utils = new MacroUtils[quotes.type]
    import utils.*

    val tTpe = TypeRepr.of[T]
    val tSymbol = tTpe.typeSymbol

    type Param = (name: Type[? <: String], tpe: Type[?])

    object extract:
      def unapply(tpe: TypeRepr): (List[Param], Type[? <: AnyKind]) = tpe match
        case MethodType(
              paramNames,
              paramTypes,
              extract(outputParams, outputTpe),
            ) =>
          (
            paramNames.zip(paramTypes).map((name, tpe) => (stringToType(name), tpe.asType)) ++ outputParams,
            outputTpe,
          )
        case other =>
          (Nil, other.asType)

    val operations =
      for
        member <- tSymbol.fieldMembers ++ tSymbol.declaredMethods
        opTpe = tTpe.memberType(member).widen
        extract(params, outputTpe) = opTpe.runtimeChecked
      yield
        val inputElems = params.map:
          case ('[type inputLabel <: String; inputLabel], '[inputTpe]) =>
            '{
              new InputElem:
                override type Type = inputTpe
                override type Label = inputLabel
                override type Metadata = Meta
            }
          case (_, _) => wontHappen
        (
          opTpe.asType,
          labelTypeOf(member, member.name),
          metaTypeOf(member),
          outputTpe,
          Expr.ofTupleFromSeq(inputElems),
        ).runtimeChecked match
          case (
                '[opTpe],
                '[type opLabel <: String; opLabel],
                '[type opMeta <: Meta; opMeta],
                '[outputTpe],
                '{ type inputElems <: Tuple; $inputElemsExpr: inputElems },
              ) =>
            '{
              new DoneOperation:
                override type Label = opLabel
                override type Metadata = opMeta
                override type InputElems = inputElems
                override type OutputType = outputTpe

                override val inputElems: InputElems = $inputElemsExpr
            }

    (
      labelTypeOf(tSymbol, tSymbol.name.stripSuffix("$")), // find a better way than stripping $
      metaTypeOf(tSymbol),
      Expr.ofTupleFromSeq(operations),
    ).runtimeChecked match
      case (
            '[type label <: String; label],
            '[type meta <: Meta; meta],
            '{ type operations <: Tuple; $operationsExpr: operations },
          ) =>
        '{
          new Done:
            override type Type = T
            override type Label = label
            override type Metadata = meta
            override type Operations = operations

            override val operations: Operations = $operationsExpr
        }
