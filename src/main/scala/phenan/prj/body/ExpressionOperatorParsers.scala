package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.Memo._

trait ExpressionOperatorParsers {
  this: ExpressionOperandParsers with CommonParsers with TwoLevelParsers =>

  def getExpressionOperatorParser (eop: ExpressionOperator, env: Environment): HParser[IRExpression] = cached((eop, env)).operator

  trait ExpressionOperatorParsersInterface {
    def operator: HParser[IRExpression]
  }

  private val cached: ((ExpressionOperator, Environment)) => ExpressionOperatorParsersInterface = mutableHashMapMemo { pair => new ExpressionOperatorParsersImpl(pair._1, pair._2) }

  private class ExpressionOperatorParsersImpl (eop: ExpressionOperator, env: Environment) extends ExpressionOperatorParsersInterface {
    lazy val operator: HParser[IRExpression] = constructParser(eop.syntax.pattern, eop.metaArgs, Nil)

    private def constructParser (pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], operands: List[IRExpression]): HParser[IRExpression] = pattern match {
      case JOperand(param, p) :: rest           => getExpressionOperandParser(param, p, binding, eop.method, env) >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JOptionalOperand(param, p) :: rest   => getExpressionOperandParser(param, p, binding, eop.method, env).?.mapOption { _.orElse(defaultArgument(param, eop.method, env)) } >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JRepetition0(param, p) :: rest       => rep0(param, p, binding, Nil) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JRepetition1(param, p) :: rest       => rep1(param, p, binding, Nil) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JMetaOperand(name, param, p) :: rest => getMetaOperandParser(param, p, binding, eop.method, env) >> {
        ma => constructParser(rest, binding + (name -> ma), operands)
      }
      case JMetaName(value, p) :: rest          => getMetaValueParser(value, p, binding, eop.method, env) ~> constructParser(rest, binding, operands)
      case JOperatorName(name) :: rest          => word(name).^ ~> constructParser(rest, binding, operands)
      case JRegexName(name) :: rest             => regex(name).^ >> { s => constructParser(rest, binding, IRStringLiteral(s, compiler) :: operands) }
      case JAndPredicate(param, p) :: rest      => getExpressionOperandParser(param, p, binding, eop.method, env).& ~> constructParser(rest, binding, operands)
      case JNotPredicate(param, p) :: rest      => getExpressionOperandParser(param, p, binding, eop.method, env).! ~> constructParser(rest, binding, operands)
      case Nil                                  => HParser.success(eop.semantics(binding, operands.reverse))
    }

    private def rep0 (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): HParser[(Map[String, MetaArgument], List[IRExpression])] = {
      rep1(param, pri, binding, args) | HParser.success((binding, args))
    }

    private def rep1 (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], args: List[IRExpression]): HParser[(Map[String, MetaArgument], List[IRExpression])] = {
      getExpressionOperandParser(param, pri, binding, eop.method, env) >> { arg => rep0(param, pri, bind(param, arg, binding), args :+ arg) }
    }

    private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = arg.staticType match {
      case Some(t) => compiler.unifier.bind(param, t, binding)
      case None    => binding
    }

    private def defaultArgument (param: JParameter, procedure: JProcedure, environment: Environment) = for {
      name   <- param.defaultArg
      method <- procedure.declaringClass.classModule.findMethod(name, environment.clazz).find(_.erasedParameterTypes == Nil)
    } yield IRDefaultArgument(method)

    private def compiler = env.clazz.compiler
  }
}
