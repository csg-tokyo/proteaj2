package phenan.prj.body

import phenan.prj._

class BodyParsers (val compiler: JCompiler)
  extends StatementParsers with ExpressionParsers with ExpressionOperatorParsers with ExpressionOperandParsers with JavaExpressionParsers with ArgumentParsers
  with LiteralParsers with LiteralOperatorParsers with LiteralOperandParsers with JavaLiteralParsers with TypeParsers with CommonParsers
