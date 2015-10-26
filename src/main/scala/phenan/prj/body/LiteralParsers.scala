package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.Memo._

trait LiteralParsers {
  this: LiteralOperatorParsers with JavaLiteralParsers with TwoLevelParsers =>

  def getLiteralParser (expected: JType, env: Environment): LParser[IRExpression] = cached((expected, env)).literal
  def getLiteralParser (expected: JType, env: Environment, priority: Option[JPriority]): LParser[IRExpression] = cached((expected, env)).literal(priority)

  trait LiteralParsersInterface {
    def literal: LParser[IRExpression]
    def literal (priority: Option[JPriority]): LParser[IRExpression]
  }

  private val cached: ((JType, Environment)) => LiteralParsersInterface = mutableHashMapMemo { pair => new LiteralParsersImpl(pair._1, pair._2) }

  private class LiteralParsersImpl (expected: JType, env: Environment) extends LiteralParsersInterface {
    def literal: LParser[IRExpression] = env.highestPriority.map(literal_cached).getOrElse(hostLiteral)
    def literal (priority: Option[JPriority]): LParser[IRExpression] = priority match {
      case Some(p) => literal_cached(p)
      case None    => hostLiteral
    }

    private lazy val hostLiteral: LParser[IRExpression] = javaLiteral(expected)

    private val literal_cached: JPriority => LParser[IRExpression] = mutableHashMapMemo(createLiteralParser)

    private def createLiteralParser (p: JPriority): LParser[IRExpression] = LParser.ref {
      env.literalOperators(expected, p).map(literalOperator(_, env)).reduceOption(_ ||| _) match {
        case Some(parser) => parser | env.nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
        case None         => env.nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
      }
    }
  }
}






