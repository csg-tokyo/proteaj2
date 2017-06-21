package phenan.prj.body

trait BodyParser {
  this: StatementParser with ExpressionParser with ExpressionOperatorParser with ExpressionOperandParser with JavaExpressionParser with ArgumentParser
    with LiteralParser with LiteralOperatorParser with LiteralOperandParser with JavaLiteralParser with TypeParser =>

  object BodyParsers
    extends StatementParsers with ExpressionParsers with ExpressionOperatorParsers with ExpressionOperandParsers with JavaExpressionParsers with ArgumentParsers
      with LiteralParsers with LiteralOperatorParsers with LiteralOperandParsers with JavaLiteralParsers with TypeParsers with CommonParsers
}
