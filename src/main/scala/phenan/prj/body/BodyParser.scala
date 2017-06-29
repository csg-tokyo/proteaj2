package phenan.prj.body

import phenan.prj.ir._

trait BodyParser {
  this: StatementParsersModule with ExpressionParsersModule with ExpressionOperatorParsersModule with ExpressionOperandParsersModule with JavaExpressionParsersModule
    with ArgumentParsersModule with LiteralParsersModule with LiteralOperatorParsersModule with LiteralOperandParsersModule with JavaLiteralParsersModule
    with TypeParsersModule with CommonParsersModule with ContextSensitiveParsersModule with NameResolvers with Environments =>

  class BodyParsers (val baseEnvironment: BaseEnvironment)
    extends StatementParsers with ExpressionParsers with ExpressionOperatorParsers with ExpressionOperandParsers with JavaExpressionParsers with ArgumentParsers
      with LiteralParsers with LiteralOperatorParsers with LiteralOperandParsers with JavaLiteralParsers with TypeParsers with CommonParsers with ContextSensitiveParsers
}
