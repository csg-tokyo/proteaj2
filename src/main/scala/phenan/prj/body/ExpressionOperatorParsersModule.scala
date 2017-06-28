package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.Memo._

trait ExpressionOperatorParsersModule {
  this: ExpressionOperandParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with Unifier with Environments with DSLEnvironments with EnvModifyStrategy
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
        case JOperand(param, p) :: rest => getExpressionOperandParser(param, p, binding, eop) >> { arg =>
          constructParser(rest, bind(param, arg, binding), arg :: operands)
        }
        case JOptionalOperand(param, p) :: rest => (getExpressionOperandParser(param, p, binding, eop) | defaultArgument(param)) >> { arg =>
          constructParser(rest, bind(param, arg, binding), arg :: operands)
        }
        case JRepetition0(param, p) :: rest => rep0(param, p, binding, Nil) >> {
          case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
        }
        case JRepetition1(param, p) :: rest => rep1(param, p, binding, Nil) >> {
          case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
        }
        case JMetaOperand(name, param, p) :: rest =>
          if (binding.contains(name)) getMetaValueExpressionParser(name, binding(name), p, binding, eop) >> { bind => constructParser(rest, bind, operands) }
          else getMetaExpressionOperandParser(param, p, binding, eop) >> { ma => constructParser(rest, binding + (name -> ma), operands) }
        case JMetaName(name, value, p) :: rest => getMetaValueExpressionParser(name, value, p, binding, eop) >> { bind => constructParser(rest, bind, operands) }
        case JOperatorName(name) :: rest => word(name).^ ~> constructParser(rest, binding, operands)
        case JRegexName(name) :: rest => regex(name.r).^ >># { s => constructParser(rest, binding, IRStringLiteral(s) :: operands) }
        case JAndPredicate(param, p) :: rest => getExpressionOperandParser(param, p, binding, eop).& ~> constructParser(rest, binding, operands)
        case JNotPredicate(param, p) :: rest => getExpressionOperandParser(param, p, binding, eop).! ~> constructParser(rest, binding, operands)
        case Nil => ContextSensitiveParser.success(eop.semantics(binding, operands.reverse))
      }

      private def rep0(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveParser[ParsedArgumentList] = {
        rep1(param, pri, binding, args) | ContextSensitiveParser.success((binding, args))
      }

      private def rep1(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveParser[ParsedArgumentList] = {
        getExpressionOperandParser(param, pri, binding, eop) >> { arg => rep0(param, pri, bind(param, arg, binding), args :+ arg) }
      }

      private def bind(param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = arg.staticType match {
        case Some(t) => bindTypeArgs(param, t, binding)
        case None => binding
      }

      private def defaultArgument (param: JParameter): ContextSensitiveParser[IRDefaultArgument] = parserEnvironment ^^? { env =>
        param.defaultArg.flatMap(name => eop.declaringClassModule.findMethod(name, env.clazz).find(_.erasedParameterTypes == Nil)).map(IRDefaultArgument)
      }
    }
  }
}