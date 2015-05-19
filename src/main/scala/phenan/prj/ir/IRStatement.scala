package phenan.prj.ir

import phenan.prj.JType

sealed trait IRStatement

case class IRBlock (statements: List[IRStatement]) extends IRStatement

case class IRLocalDeclaration (localType: JType, declarators: List[IRVariableDeclarator]) extends IRStatement

case class IRVariableDeclarator (name: String, dim: Int, init: IRExpression)

case class IRExpressionStatement (expression: IRExpression) extends IRStatement