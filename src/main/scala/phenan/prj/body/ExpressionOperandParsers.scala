package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

trait ExpressionOperandParsers {
  this: ExpressionParsers with TypeParsers with TwoLevelParsers =>

  def getExpressionOperandParser (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[IRExpression] = {
    getExpressionOperandParserOpt(param, pri, binding, procedure, env).getOrElse(HParser.failure("fail to parse argument expression"))
  }

  def getMetaOperandParser (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[MetaArgument] = {
    ExpectedTypeInferencer.expected(param, binding, procedure) match {
      case Some(e) if env.clazz.compiler.typeLoader.typeType.contains(e) => getTypeParsers(env.resolver).metaValue
      case Some(e) => getExpressionOperandParser(param, pri, binding, procedure, env) ^^  { ConcreteMetaValue(_, param) }
      case None    => HParser.failure("fail to parse meta expression")
    }
  }

  def getMetaValueParser (mv: MetaArgument, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): HParser[MetaArgument] = mv match {
    case t: JRefType  => getTypeParsers(env.resolver).refType ^? { case v if t == v => t }
    case w: JWildcard => getTypeParsers(env.resolver).wildcard ^? { case v if w == v => w }
    case r: MetaVariableRef   => getTypeParsers(env.resolver).metaVariable ^? { case v if r == v => r }
    case c: ConcreteMetaValue => getExpressionOperandParser(c.parameter, pri, binding, procedure, env) ^? { case v if c.ast == v => c }
    case _: MetaValueWildcard => HParser.failure("meta value wildcard cannot be placed in operator pattern")
  }

  private def getExpressionOperandParserOpt (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): Option[HParser[IRExpression]] = {
    for {
      expectedType <- ExpectedTypeInferencer.expected(param, binding, procedure)
      contexts     <- IRContextRef.createRefs(param.contexts, binding)
    } yield getExpressionParser(expectedType, env.withContexts(contexts), env.getPriority(pri, procedure)) ^^ { _.withContexts(contexts) }
  }
}
