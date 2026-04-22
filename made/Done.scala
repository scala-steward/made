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

  final type Args = Tuple.Map[InputElems, InputElem.ExtractOf]

  def inputElems: InputElems

  /** The enclosing type that declares this operation — equals [[Done.Type]] of the parent [[Done]] mirror. */
  type OuterType

  /** The return type of the operation. */
  type OutputType

  /**
   * Invokes the underlying member on an instance of the enclosing type.
   *
   * Arguments in `args` correspond positionally to [[InputElems]] (multiple parameter
   * lists are flattened). Each argument is unboxed via `asInstanceOf` to its declared
   * parameter type, so supplying a value of the wrong type will fail at runtime.
   *
   * @param outer the instance on which to invoke the member
   * @param args  the positional arguments, in [[InputElems]] order
   */
  def apply(outer: OuterType, args: Args): OutputType

object DoneOperation:
  type Of[T] = DoneOperation { type OuterType = T }
  type ExtractOf[X /* <: DoneOperation */ ] = X match
    case DoneOperation.Of[t] => t

  /**
   * Mix-in providing an ergonomic `apply(outer, arg)` shortcut for operations with exactly
   * one input. The self-type refinement requires the mixing class to have
   * `Args = Arg *: EmptyTuple`.
   */
  trait SingleApply extends DoneOperation:
    self: DoneOperation { type Args = Arg *: EmptyTuple } =>
    type Arg

    final def apply(outer: OuterType, arg: Arg): OutputType = this.apply(outer, arg *: EmptyTuple)

  /**
   * Mix-in providing an ergonomic `apply(outer)` shortcut for parameterless operations.
   * The self-type refinement requires the mixing class to have `Args = EmptyTuple`.
   */
  trait EmptyApply extends DoneOperation:
    self: DoneOperation { type Args = EmptyTuple } =>

    final def apply(outer: OuterType): OutputType = this.apply(outer, EmptyTuple)

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

object InputElem:
  type Of[T] = InputElem { type Type = T }
  type ExtractOf[X /* <: InputElem */ ] = X match
    case InputElem.Of[t] => t
  type OuterOf[T] = InputElem { type OuterType = T }

object Done:
  type Of[T] = Done { type Type = T }

  /**
   * Type-safe entry point for [[DoneOperation.apply]] — enforces at compile time
   * that `op` was derived from the same mirror (its [[DoneOperation.OuterType]]
   * equals the mirror's [[Done.Type]]).
   */
  extension [T](done: Done.Of[T])
    def invoke(
      op: DoneOperation { type OuterType = T },
      target: T,
      args: op.Args,
    ): op.OutputType = op.apply(target, args)

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

  // workaround for https://github.com/scala/scala3/issues/25245
  private sealed trait DoneOperationWorkaround[Outer] extends DoneOperation:
    final type OuterType = Outer

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

    def invokeExpr[Out: Type](
      member: Symbol,
      memberTpe: TypeRepr,
      outer: Expr[T],
      args: Expr[Tuple],
    ): Expr[Out] =
      def go(tpe: TypeRepr, idx: Int): List[List[Term]] = tpe match
        case MethodType(_, paramTypes, result) =>
          val argTerms = paramTypes.map: pTpe =>
            pTpe.asType match
              case '[t] => '{ $args.productElement(${ Expr(idx) }).asInstanceOf[t] }.asTerm
          argTerms :: go(result, idx + 1)
        case _ => Nil

      val argLists = go(memberTpe, 0)
      val sel = outer.asTerm.select(member)
      val applied = if argLists.isEmpty then sel else sel.appliedToArgss(argLists)
      applied.asExprOf[Out]

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
            params match
              case Nil =>
                '{
                  new DoneOperationWorkaround[T] with DoneOperation.EmptyApply:
                    override type Label = opLabel
                    override type Metadata = opMeta
                    override type InputElems = inputElems
                    override type OutputType = outputTpe

                    override val inputElems: InputElems = $inputElemsExpr
                    override def apply(outer: T, args: Args): outputTpe =
                      ${ invokeExpr[outputTpe](member, opTpe, '{ outer }, '{ args.asInstanceOf[Tuple] }) }
                  : DoneOperation.EmptyApply {
                    type Label = opLabel
                    type Metadata = opMeta
                    type InputElems = inputElems
                    type OuterType = T
                    type OutputType = outputTpe
                  }
                }
              case (_, '[argT]) :: Nil =>
                '{
                  new DoneOperationWorkaround[T] with DoneOperation.SingleApply:
                    override type Label = opLabel
                    override type Metadata = opMeta
                    override type InputElems = inputElems
                    override type OutputType = outputTpe
                    override type Arg = argT

                    override val inputElems: InputElems = $inputElemsExpr
                    override def apply(outer: T, args: Args): outputTpe =
                      ${ invokeExpr[outputTpe](member, opTpe, '{ outer }, '{ args.asInstanceOf[Tuple] }) }
                  : DoneOperation.SingleApply {
                    type Label = opLabel
                    type Metadata = opMeta
                    type InputElems = inputElems
                    type OuterType = T
                    type OutputType = outputTpe
                    type Arg = argT
                  }
                }
              case _ =>
                '{
                  new DoneOperationWorkaround[T]:
                    override type Label = opLabel
                    override type Metadata = opMeta
                    override type InputElems = inputElems
                    override type OutputType = outputTpe

                    override val inputElems: InputElems = $inputElemsExpr
                    override def apply(outer: T, args: Args): outputTpe =
                      ${ invokeExpr[outputTpe](member, opTpe, '{ outer }, '{ args.asInstanceOf[Tuple] }) }
                  : DoneOperation {
                    type Label = opLabel
                    type Metadata = opMeta
                    type InputElems = inputElems
                    type OuterType = T
                    type OutputType = outputTpe
                  }
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
