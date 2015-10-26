package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

trait ArgumentParsers {
  this: ExpressionParsers with CommonParsers with TwoLevelParsers =>

  def getArgumentsParser (procedure: JProcedure, env: Environment): HParser[(Map[String, MetaArgument], List[IRExpression])] = {
    '(' ~> arguments_abbrev(procedure.parameterTypes, Map.empty, procedure, env, Nil) <~ ')'
  }

  def getArgumentsParser (procedure: JProcedure, binding: Map[String, MetaArgument], env: Environment): HParser[List[IRExpression]] = {
    '(' ~> arguments_canonical(procedure.parameterTypes, binding, env, Nil) <~ ')'
  }

  private def arguments_abbrev (parameters: List[JParameter], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment, args: List[IRExpression]): HParser[(Map[String, MetaArgument], List[IRExpression])] = parameters match {
    case Nil           => HParser.success((binding, args))
    case param :: Nil  => argument_abbrev(param, binding, procedure, env) ^^ { arg => (bind(param, arg, binding, env), args :+ arg) }
    case param :: rest => argument_abbrev(param, binding, procedure, env) >> { arg => ',' ~> arguments_abbrev(rest, bind(param, arg, binding, env), procedure, env, args :+ arg) }
  }

  private def arguments_canonical (paramTypes: List[JParameter], binding: Map[String, MetaArgument], env: Environment, args: List[IRExpression]): HParser[List[IRExpression]] = paramTypes match {
    case Nil           => HParser.success(Nil)
    case param :: Nil  => argument_canonical(param, binding, env) ^^ { arg => args :+ arg }
    case param :: rest => argument_canonical(param, binding, env) <~ ',' >> { arg => arguments_canonical(rest, bind(param, arg, binding, env), env, args :+ arg) }
  }

  private def argument_abbrev (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment) = {
    argument_abbrev_opt(param, binding, procedure, env).getOrElse(HParser.failure("fail to parse argument expression"))
  }

  private def argument_abbrev_opt (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment) = {
    for {
      expectedType <- ExpectedTypeInferencer.expected(param, binding, procedure)
      contexts     <- IRContextRef.createRefs(param.contexts, binding)
    } yield getExpressionParser(expectedType, env.withContexts(contexts), env.highestPriority) ^^ { _.withContexts(contexts) }
  }

  private def argument_canonical (param: JParameter, binding: Map[String, MetaArgument], env: Environment) = {
    argument_canonical_opt(param, binding, env).getOrElse(HParser.failure("fail to parse argument expression"))
  }

  private def argument_canonical_opt (param: JParameter, binding: Map[String, MetaArgument], env: Environment) = {
    for {
      expectedType <- param.genericType.bind(binding)
      contexts     <- IRContextRef.createRefs(param.contexts, binding)
    } yield getExpressionParser(expectedType, env.withContexts(contexts), env.highestPriority) ^^ { _.withContexts(contexts) }
  }

  private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument], env: Environment) = arg.staticType match {
    case Some(t) => env.clazz.compiler.unifier.bind(param, t, binding)
    case None    => binding
  }
}
