package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.Memo._

trait ExpressionParsersModule {
  this: ExpressionOperatorParsersModule with JavaExpressionParsersModule with LiteralParsersModule
    with CommonParsersModule with ContextSensitiveParsersModule
    with JTypeLoader with Environments with EnvModifyStrategy with IRExpressions with JModules =>

  trait ExpressionParsers {
    this: ExpressionOperatorParsers with JavaExpressionParsers with LiteralParsers with CommonParsers with ContextSensitiveParsers =>

    def getExpressionParser(expected: JType): ContextSensitiveParser[IRExpression] = cached(expected).expression

    def getExpressionParser(expected: JType, parameterPriority: Option[JPriority], enclosingPriority: JPriority): ContextSensitiveParser[IRExpression] = {
      cached(expected).expression(parameterPriority, enclosingPriority)
    }

    trait ExpressionParsersInterface {
      def expression: ContextSensitiveParser[IRExpression]

      def expression(parameterPriority: Option[JPriority], enclosingPriority: JPriority): ContextSensitiveParser[IRExpression]
    }

    private val cached: JType => ExpressionParsersInterface = mutableHashMapMemo { new ExpressionParsersImpl(_) }

    private class ExpressionParsersImpl (expected: JType) extends ExpressionParsersInterface {
      def expression: ContextSensitiveParser[IRExpression] = highestPriority.map(expression_cached).getOrElse(hostExpression)

      def expression (parameterPriority: Option[JPriority], enclosingPriority: JPriority): ContextSensitiveParser[IRExpression] = {
        parameterPriority.orElse(nextPriority(enclosingPriority)) match {
          case Some(p) => expression_cached(p)
          case None    => hostExpression
        }
      }

      lazy val hostExpression: ContextSensitiveParser[IRExpression] = javaExpression | parenthesized | literal

      lazy val javaExpression: ContextSensitiveParser[IRExpression] = javaExpressionParser ^? { case e if e.staticType.exists(_ <:< expected) || expected == voidType => e }

      lazy val parenthesized: ContextSensitiveParser[IRExpression] = '(' ~> expression <~ ')'

      lazy val literal: ContextSensitiveParser[IRExpression] = getLiteralParser(expected).^

      private val expression_cached: JPriority => ContextSensitiveParser[IRExpression] = mutableHashMapMemo(createExpressionParser)

      private def createExpressionParser(p: JPriority): ContextSensitiveParser[IRExpression] = ContextSensitiveParser { env =>
        env.expressionOperators(expected, p).map(getExpressionOperatorParser).reduceOption(_ ||| _) match {
          case Some(parser) => parser | nextPriority(p).map(expression_cached).getOrElse(hostExpression)
          case None => nextPriority(p).map(expression_cached).getOrElse(hostExpression)
        }
      }
    }
  }
}







