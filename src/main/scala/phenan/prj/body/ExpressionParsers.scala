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

    lazy val literal = getLiteralParser(expected, env).^

    private val expression_cached: JPriority => HParser[IRExpression] = mutableHashMapMemo(createExpressionParser)

    private def createExpressionParser (p: JPriority): HParser[IRExpression] = HParser.ref {
      env.expressionOperators(expected, p).map(getExpressionOperatorParser(_, env)).reduceOption(_ ||| _) match {
        case Some(parser) => parser | env.nextPriority(p).map(expression_cached).getOrElse(hostExpression)
        case None         => env.nextPriority(p).map(expression_cached).getOrElse(hostExpression)
      }
    }

    private def compiler = env.clazz.compiler
  }
}








