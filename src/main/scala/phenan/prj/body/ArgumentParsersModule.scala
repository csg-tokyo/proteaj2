package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait ArgumentParsersModule {
  this: ExpressionParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with ExpectedTypeInferencer with Unifier with Environments with EnvModifyStrategy
    with JMembers with JModules with IRExpressions =>

  trait ArgumentParsers {
    this: ExpressionParsers with CommonParsers with ContextSensitiveParsers =>

    def getArgumentsParser(procedure: JProcedure): ContextSensitiveParser[ParsedArgumentList] = {
      '(' ~> arguments_abbrev(procedure.parameterTypes, Map.empty, procedure, Nil) <~ ')'
    }

    def getArgumentsParser(procedure: JProcedure, binding: Map[String, MetaArgument]): ContextSensitiveParser[List[IRExpression]] = {
      '(' ~> arguments_canonical(procedure.parameterTypes, binding, Nil) <~ ')'
    }

    private def arguments_abbrev(parameters: List[JParameter], binding: Map[String, MetaArgument], procedure: JProcedure, args: List[IRExpression]): ContextSensitiveParser[ParsedArgumentList] = parameters match {
      case Nil => ContextSensitiveParser.success((binding, args))
      case param :: Nil => argument_abbrev(param, binding, procedure) ^^ { arg => (bind(param, arg, binding), args :+ arg) }
      case param :: rest => argument_abbrev(param, binding, procedure) >> { arg => ',' ~> arguments_abbrev(rest, bind(param, arg, binding), procedure, args :+ arg) }
    }

    private def arguments_canonical(paramTypes: List[JParameter], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveParser[List[IRExpression]] = paramTypes match {
      case Nil => ContextSensitiveParser.success[List[IRExpression]](Nil)
      case param :: Nil => argument_canonical(param, binding) ^^ { arg => args :+ arg }
      case param :: rest => argument_canonical(param, binding) <~ ',' >> { arg => arguments_canonical(rest, bind(param, arg, binding), args :+ arg) }
    }

    private def argument_abbrev(param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure) = {
      argument_abbrev_opt(param, binding, procedure).getOrElse(ContextSensitiveParser.failure[IRExpression]("fail to parse argument expression"))
    }

    private def argument_abbrev_opt(param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure) = {
      for {
        expectedType <- inferExpectedType(param, binding, procedure)
        contexts     <- inferContexts(param.contexts, binding, procedure)
      } yield getExpressionParser(expectedType).withLocalContexts(contexts)
    }

    private def argument_canonical(param: JParameter, binding: Map[String, MetaArgument]) = {
      argument_canonical_opt(param, binding).getOrElse(ContextSensitiveParser.failure[IRExpression]("fail to parse argument expression"))
    }

    private def argument_canonical_opt(param: JParameter, binding: Map[String, MetaArgument]) = for {
      expectedType <- param.genericType.bind(binding)
      contexts <- param.contexts.traverse(_.bind(binding).collect { case obj: JObjectType => IRContextRef(obj) })
    } yield getExpressionParser(expectedType).withLocalContexts(contexts)

    private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = arg.staticType match {
      case Some(t) => bindTypeArgs(param, t, binding)
      case None    => binding
    }
  }
}