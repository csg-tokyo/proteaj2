package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

trait LiteralOperandParser {
  this: LiteralParser with ExpectedTypeInferencer with Environments with IRExpressions with JModules with JMembers =>
  trait LiteralOperandParsers {
    this: LiteralParsers with TwoLevelParsers =>

    def getLiteralOperandParser(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): LParser[IRExpression] = {
      literalOperandOpt(param, pri, binding, procedure, env).getOrElse(LParser.failure("fail to parse argument expression"))
    }

    def getMetaLiteralOperandParser(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): LParser[MetaArgument] = {
      inferExpectedType(param, binding, procedure) match {
        case Some(e) => getMetaArgumentParser(e, pri, binding, procedure, env)
        case None => LParser.failure("fail to parse meta expression")
      }
    }

    def getMetaValueLiteralParser(name: String, mv: MetaArgument, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): LParser[Map[String, MetaArgument]] = mv match {
      case c: ConcreteMetaValue => getMetaArgumentParser(c.valueType, pri, binding, procedure, env) ^? { case v if c.ast == v => c } ^^^ binding
      case u: JUnboundMetaVariable => getMetaArgumentParser(u.valueType, pri, binding, procedure, env) ^^ { arg => binding + (name -> arg) }
      case _: JRefType | _: JWildcard | _: MetaVariableRef => LParser.failure("type name cannot be used in a literal")
    }

    private def getMetaArgumentParser(metaType: JType, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): LParser[MetaArgument] = {
      getLiteralParser(metaType, env, env.getPriority(pri, procedure)) ^^ { arg => ConcreteMetaValue(arg, metaType) }
    }

    private def literalOperandOpt(param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): Option[LParser[IRExpression]] = {
      for {
        expectedType <- inferExpectedType(param, binding, procedure)
        contexts     <- inferContexts(param.contexts, binding, procedure)
      } yield getLiteralParser(expectedType, env.withContexts(contexts), env.getPriority(pri, procedure)) ^^ {
        _.withContexts(contexts)
      }
    }
  }

}