package phenan.prj.body

import phenan.prj.{Unifier, _}
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait ArgumentParser {
  this: ExpectedTypeInferencer with Unifier with ExpressionParser with Environments with JMembers with JModules with IRExpressions =>
  trait ArgumentParsers {
    this: ExpressionParsers with CommonParsers with TwoLevelParsers =>

    def getArgumentsParser(procedure: JProcedure, env: Environment): HParser[(Map[String, MetaArgument], List[IRExpression])] = {
      '(' ~> arguments_abbrev(procedure.parameterTypes, Map.empty, procedure, env, Nil) <~ ')'
    }

    def getArgumentsParser(procedure: JProcedure, binding: Map[String, MetaArgument], env: Environment): HParser[List[IRExpression]] = {
      '(' ~> arguments_canonical(procedure.parameterTypes, binding, env, Nil) <~ ')'
    }

    private def arguments_abbrev(parameters: List[JParameter], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment, args: List[IRExpression]): HParser[(Map[String, MetaArgument], List[IRExpression])] = parameters match {
      case Nil => HParser.success((binding, args))
      case param :: Nil => argument_abbrev(param, binding, procedure, env) ^^ { arg => (bind(param, arg, binding), args :+ arg) }
      case param :: rest => argument_abbrev(param, binding, procedure, env) >> { arg => ',' ~> arguments_abbrev(rest, bind(param, arg, binding), procedure, env, args :+ arg) }
    }

    private def arguments_canonical(paramTypes: List[JParameter], binding: Map[String, MetaArgument], env: Environment, args: List[IRExpression]): HParser[List[IRExpression]] = paramTypes match {
      case Nil => HParser.success(Nil)
      case param :: Nil => argument_canonical(param, binding, env) ^^ { arg => args :+ arg }
      case param :: rest => argument_canonical(param, binding, env) <~ ',' >> { arg => arguments_canonical(rest, bind(param, arg, binding), env, args :+ arg) }
    }

    private def argument_abbrev(param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment) = {
      argument_abbrev_opt(param, binding, procedure, env).getOrElse(HParser.failure("fail to parse argument expression"))
    }

    private def argument_abbrev_opt(param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment) = {
      for {
        expectedType <- inferExpectedType(param, binding, procedure)
        contexts     <- inferContexts(param.contexts, binding, procedure)
      } yield getExpressionParser(expectedType, env.withContexts(contexts), env.highestPriority) ^^ {
        _.withContexts(contexts)
      }
    }

    private def argument_canonical(param: JParameter, binding: Map[String, MetaArgument], env: Environment) = {
      argument_canonical_opt(param, binding, env).getOrElse(HParser.failure("fail to parse argument expression"))
    }

    private def argument_canonical_opt(param: JParameter, binding: Map[String, MetaArgument], env: Environment) = for {
      expectedType <- param.genericType.bind(binding)
      contexts <- param.contexts.traverse(_.bind(binding).collect { case obj: JObjectType => IRContextRef(obj) })
    } yield getExpressionParser(expectedType, env.withContexts(contexts), env.highestPriority) ^^ {
      _.withContexts(contexts)
    }

    private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = arg.staticType match {
      case Some(t) => bindTypeArgs(param, t, binding)
      case None    => binding
    }
  }
}