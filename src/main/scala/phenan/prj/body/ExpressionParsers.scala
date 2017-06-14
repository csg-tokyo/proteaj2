package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.Memo._

trait ExpressionParsers {
  this: ExpressionOperatorParsers with JavaExpressionParsers with LiteralParsers with CommonParsers with TwoLevelParsers =>

  def getExpressionParser (expected: JType, env: Environment): HParser[IRExpression] = cached((expected, env)).expression
  def getExpressionParser (expected: JType, env: Environment, priority: Option[JPriority]): HParser[IRExpression] = cached((expected, env)).expression(priority)

  trait ExpressionParsersInterface {
    def expression: HParser[IRExpression]
    def expression (priority: Option[JPriority]): HParser[IRExpression]
  }

  private val cached: ((JType, Environment)) => ExpressionParsersInterface = mutableHashMapMemo { pair => new ExpressionParsersImpl(pair._1, pair._2) }

  private class ExpressionParsersImpl (expected: JType, env: Environment) extends ExpressionParsersInterface {
    def expression: HParser[IRExpression] = env.highestPriority.map(expression_cached).getOrElse(hostExpression)

    def expression (priority: Option[JPriority]): HParser[IRExpression] = priority match {
      case Some(p) => expression_cached(p)
      case None    => hostExpression
    }

    lazy val hostExpression: HParser[IRExpression] = javaExpression | parenthesized | literal

    lazy val javaExpression: HParser[IRExpression] = getJavaExpressionParser(env) ^? { case e if e.staticType.exists(_ <:< expected) || expected == compiler.typeLoader.void => e }

    lazy val parenthesized: HParser[IRExpression] = '(' ~> expression <~ ')'

    lazy val literal: HParser[IRExpression] = getLiteralParser(expected, env).^

    private val expression_cached: JPriority => HParser[IRExpression] = mutableHashMapMemo(createExpressionParser)

    private def createExpressionParser (p: JPriority): HParser[IRExpression] = expressionOperatorsParser(p) | expression(env.nextPriority(p))

    private def expressionOperatorsParser (p: JPriority): HParser[IRExpression] = {
      env.expressionOperators(expected, p).map(getExpressionOperatorParser(_, env)).foldLeft[HParser[IRExpression]](HParser.failure("there is no operator for " + expected.name + " [" + p.name + "]"))(_ ||| _)
    }

    private def compiler = env.clazz.compiler
  }
}








