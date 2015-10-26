package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

trait LiteralOperandParsers {
  this: LiteralParsers with TwoLevelParsers =>

  def literalOperand (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): LParser[IRExpression] = {
    literalOperandOpt(param, pri, binding, procedure, env).getOrElse(LParser.failure("fail to parse argument expression"))
  }

  private def literalOperandOpt (param: JParameter, pri: Option[JPriority], binding: Map[String, MetaArgument], procedure: JProcedure, env: Environment): Option[LParser[IRExpression]] = {
    for {
      expectedType <- ExpectedTypeInferencer.expected(param, binding, procedure)
      contexts     <- IRContextRef.createRefs(param.contexts, binding)
    } yield getLiteralParser(expectedType, env.withContexts(contexts), env.getPriority(pri, procedure)) ^^ { _.withContexts(contexts) }
  }
}
