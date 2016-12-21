package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

trait ExpressionOperandParsers {
  this: ExpressionParsers with TypeParsers with TwoLevelParsers with StatementParsers =>

  def getExpressionOperandParser (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[IRExpression] = {
    getExpressionOperandParserOpt(param, pri, binding, procedure, env).getOrElse(HParser.failure("fail to parse argument expression"))
  }

  def getMetaExpressionOperandParser (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[MetaArgument] = {
    ExpectedTypeInferencer.expected(param, binding, procedure) match {
      case Some(e) => getMetaArgumentParser(e, pri, binding, procedure, env)
      case None    => HParser.failure("fail to parse meta expression")
    }
  }

  def getMetaValueExpressionParser (name: String, mv: MetaArgument, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[Map[String, MetaArgument]] = mv match {
    case t: JRefType  => getTypeParsers(env.resolver).refType ^? { case v if t == v => t } ^^^ binding
    case w: JWildcard => getTypeParsers(env.resolver).wildcard ^? { case v if w == v => w } ^^^ binding
    case r: MetaVariableRef   => getTypeParsers(env.resolver).metaVariable ^? { case v if r == v => r } ^^^ binding
    case c: ConcreteMetaValue => getMetaArgumentParser(c.valueType, pri, binding, procedure, env) ^? { case v if c == v => v } ^^^ binding
    case u: JUnboundMetaVariable => getMetaArgumentParser(u.valueType, pri, binding, procedure, env) ^^ { arg => binding + (name -> arg) }
  }

  private def getMetaArgumentParser (metaType: JType, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[MetaArgument] = {
    if (env.clazz.compiler.typeLoader.typeType.contains(metaType)) getTypeParsers(env.resolver).metaValue
    else getExpressionParser(metaType, env, env.getPriority(pri, procedure)) ^^ { arg => ConcreteMetaValue(arg, metaType) }
  }

  private def getExpressionOperandParserOpt (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): Option[HParser[IRExpression]] = for {
    expectedType <- ExpectedTypeInferencer.expected(param, binding, procedure)
    contexts     <- ExpectedTypeInferencer.contexts(param.contexts, binding, procedure)
  } yield {
    if (compiler.typeLoader.void.boxed.contains(expectedType) && contexts.nonEmpty) {
      val boxed = getExpressionParser(expectedType, env.withContexts(contexts), env.getPriority(pri, procedure)) ^^ { _.withContexts(contexts) }
      val unboxed = getExpressionParser(compiler.typeLoader.void, env.withContexts(contexts), env.getPriority(pri, procedure)) ^^ { e => IRStatementExpression(IRExpressionStatement(e), contexts, expectedType) }
      val block = getStatementParsers(compiler.typeLoader.void, env.withContexts(contexts)).block ^^ { b => IRStatementExpression(b, contexts, expectedType) }
      boxed | unboxed | block
    }
    else getExpressionParser(expectedType, env.withContexts(contexts), env.getPriority(pri, procedure)) ^^ { _.withContexts(contexts) }
  }
}
