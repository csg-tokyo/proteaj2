package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.Memo._

trait ExpressionOperatorParsersModule {
  this: ExpressionOperandParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with Environments with DSLEnvironments with EnvModifyStrategy
    with IRs with IRExpressions with Syntax with JModules with JMembers with JErasedTypes =>

  trait ExpressionOperatorParsers {
    this: ExpressionOperandParsers with CommonParsers with ContextSensitiveParsers =>

    def getExpressionOperatorParser(eop: ExpressionOperator): ContextSensitiveParser[IRExpression] = cached(eop).operator

    trait ExpressionOperatorParsersInterface {
      def operator: ContextSensitiveParser[IRExpression]
    }

    private val cached: ExpressionOperator => ExpressionOperatorParsersInterface = mutableHashMapMemo { new ExpressionOperatorParsersImpl(_) }

    private class ExpressionOperatorParsersImpl (eop: ExpressionOperator) extends ExpressionOperatorParsersInterface {
      lazy val operator: ContextSensitiveParser[IRExpression] = constructParser(eop.syntax.pattern, eop.metaArgs, Nil)

      private def constructParser(pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], operands: List[IRExpression]): ContextSensitiveParser[IRExpression] = pattern match {
        case JOperand(param, p) :: rest =>
          getExpressionOperandParser(param, p, binding, eop) >> {
            case (bind, arg) => constructParser(rest, bind, arg :: operands)
          }
        case JOptionalOperand(param, p) :: rest =>
          (getExpressionOperandParser(param, p, binding, eop) | defaultArgumentParser(param, binding).^#) >> {
            case (bind, arg) => constructParser(rest, bind, arg :: operands)
          }
        case JRepetition0(param, p) :: rest =>
          rep0(param, p, binding, Nil).mapOption {
            case (bind, args) => param.genericType.bind(bind).map(bind -> IRVariableArguments(args, _))
          } >> {
            case (bind, vargs) => constructParser(rest, bind, vargs :: operands)
          }
        case JRepetition1(param, p) :: rest =>
          rep1(param, p, binding, Nil).mapOption {
            case (bind, args) => param.genericType.bind(bind).map(bind -> IRVariableArguments(args, _))
          } >> {
            case (bind, vargs) => constructParser(rest, bind, vargs :: operands)
          }
        case JMetaOperand(name, param, p) :: rest =>
          if (binding.contains(name)) getMetaValueExpressionParser(name, binding(name), p, binding, eop) >> { bind => constructParser(rest, bind, operands) }
          else getMetaExpressionOperandParser(param, p, binding, eop) >> { ma => constructParser(rest, binding + (name -> ma), operands) }
        case JMetaName(name, value, p) :: rest =>
          getMetaValueExpressionParser(name, value, p, binding, eop) >> { bind => constructParser(rest, bind, operands) }
        case JOperatorName(name) :: rest =>
          word(name).^ ~> constructParser(rest, binding, operands)
        case JRegexName(name) :: rest =>
          regex(name.r).^ >># { s => constructParser(rest, binding, IRStringLiteral(s) :: operands) }
        case JAndPredicate(param, p) :: rest =>
          getExpressionOperandParser(param, p, binding, eop).& ~> constructParser(rest, binding, operands)
        case JNotPredicate(param, p) :: rest =>
          getExpressionOperandParser(param, p, binding, eop).! ~> constructParser(rest, binding, operands)
        case Nil =>
          ContextSensitiveParser.success(eop.semantics(binding, operands.reverse))
      }

      private def rep0(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveParser[ParsedArgumentList] = {
        rep1(param, pri, binding, args) | ContextSensitiveParser.success((binding, args))
      }

      private def rep1(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveParser[ParsedArgumentList] = {
        getExpressionOperandParser(param, pri, binding, eop) >> {
          case (bind, arg) => rep0(param, pri, bind, args :+ arg)
        }
      }

      private def defaultArgumentParser (param: JParameter, binding: Map[String, MetaArgument]): ContextFreeParser[ParsedArgument] = defaultArgument(param, binding) match {
        case Some(value) => ContextFreeParser.success((binding, value))
        case None        => ContextFreeParser.failure("default argument is not found")
      }

      private def defaultArgument (param: JParameter, binding: Map[String, MetaArgument]): Option[IRDefaultArgument] = for {
        name   <- param.defaultArg
        method <- eop.declaringClassModule.findMethod(name, declaringModule).find(_.erasedParameterTypes == Nil)
      } yield IRDefaultArgument(method, binding)
    }
  }
}