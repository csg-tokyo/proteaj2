package phenan.prj.ir

sealed trait IRExpression

case class IROperation (syntax: Syntax, operands: List[IRExpression]) extends IRExpression
