package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.Memo._

trait LiteralParsersModule {
  this: LiteralOperatorParsersModule with JavaLiteralParsersModule
    with CommonParsersModule with ContextSensitiveParsersModule
    with Environments with EnvModifyStrategy with IRExpressions with JModules =>

  trait LiteralParsers {
    this: LiteralOperatorParsers with JavaLiteralParsers with CommonParsers with ContextSensitiveParsers =>

    def getLiteralParser(expected: JType): ContextSensitiveScanner[IRExpression] = cached(expected).literal

    def getLiteralParser(expected: JType, parameterPriority: Option[JPriority], enclosingPriority: JPriority): ContextSensitiveScanner[IRExpression] = {
      cached(expected).literal(parameterPriority, enclosingPriority)
    }

    trait LiteralParsersInterface {
      def literal: ContextSensitiveScanner[IRExpression]

      def literal (parameterPriority: Option[JPriority], enclosingPriority: JPriority): ContextSensitiveScanner[IRExpression]
    }

    private val cached: JType => LiteralParsersInterface = mutableHashMapMemo { new LiteralParsersImpl(_) }

    private class LiteralParsersImpl(expected: JType) extends LiteralParsersInterface {
      def literal: ContextSensitiveScanner[IRExpression] = highestPriority.map(literal_cached).getOrElse(hostLiteral)

      def literal (parameterPriority: Option[JPriority], enclosingPriority: JPriority): ContextSensitiveScanner[IRExpression] = {
        parameterPriority.orElse(nextPriority(enclosingPriority)) match {
          case Some(p) => literal_cached(p)
          case None    => hostLiteral
        }
      }

      private lazy val hostLiteral: ContextSensitiveScanner[IRExpression] = javaLiteral(expected).^#

      private val literal_cached: JPriority => ContextSensitiveScanner[IRExpression] = mutableHashMapMemo(createLiteralParser)

      private def createLiteralParser(p: JPriority): ContextSensitiveScanner[IRExpression] = ContextSensitiveScanner { env =>
        env.literalOperators(expected, p).map(getLiteralOperatorParser).reduceOption(_ ||| _) match {
          case Some(parser) => parser | nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
          case None => nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
        }
      }
    }
  }
}





