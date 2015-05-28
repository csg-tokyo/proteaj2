package phenan.prj.ir

import phenan.prj.JSyntax

sealed trait IRExpression

case class IROperation (syntax: JSyntax, operands: List[IRExpression]) extends IRExpression
