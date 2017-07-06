package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait ArgumentParsersModule {
  this: ExpressionParsersModule with CommonParsersModule with ContextSensitiveParsersModule
    with ExpectedTypeInferencer with Environments with EnvModifyStrategy
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
      case Nil =>
        ContextSensitiveParser.success((binding, args))
      case param :: Nil =>
        argument_abbrev(param, binding, procedure) ^^ { case (bind, arg) => (bind, args :+ arg) }
      case param :: rest =>
        argument_abbrev(param, binding, procedure) <~ ',' >> { case (bind, arg) => arguments_abbrev(rest, bind, procedure, args :+ arg) }
    }

    private def arguments_canonical(paramTypes: List[JParameter], binding: Map[String, MetaArgument], args: List[IRExpression]): ContextSensitiveParser[List[IRExpression]] = paramTypes match {
      case Nil =>
        ContextSensitiveParser.success[List[IRExpression]](Nil)
      case param :: Nil =>
        argument_canonical(param, binding) ^^ { case (_, arg) => args :+ arg }
      case param :: rest =>
        argument_canonical(param, binding) <~ ',' >> { case (bind, arg) => arguments_canonical(rest, bind, args :+ arg) }
    }

    private def argument_abbrev(param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure): ContextSensitiveParser[ParsedArgument] = {
      val parser = for {
        expectedType <- inferExpectedType(param, binding, procedure)
        contexts     <- inferContexts(param.contexts, binding, procedure)
      } yield getExpressionParser(expectedType).withLocalContexts(contexts).argumentFor(param, binding)

      parser.getOrElse(ContextSensitiveParser.failure[ParsedArgument]("fail to parse argument expression"))
    }

    private def argument_canonical(param: JParameter, binding: Map[String, MetaArgument]): ContextSensitiveParser[ParsedArgument] = {
      val parser = for {
        expectedType <- param.genericType.bind(binding)
        contexts     <- param.contexts.traverse(_.bind(binding).collect { case obj: JObjectType => IRContextRef(obj) })
      } yield getExpressionParser(expectedType).withLocalContexts(contexts).argumentFor(param, binding)

      parser.getOrElse(ContextSensitiveParser.failure[ParsedArgument]("fail to parse argument expression"))
    }
  }
}