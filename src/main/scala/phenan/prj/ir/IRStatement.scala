package phenan.prj.ir

import phenan.prj._

case class IRMethodBody (block: IRBlock)

case class IRConstructorBody (constructorCall: Option[IRExplicitConstructorCall], statements: List[IRStatement])

case class IRInitializerBody (block: IRBlock)

sealed trait IRStatement

case class IRBlock (statements: List[IRStatement]) extends IRStatement

case class IRLocalDeclarationStatement (declaration: IRLocalDeclaration) extends IRStatement {
  override def toString: String = declaration.toString + ';'
}

case class IRLocalDeclaration (localType: JType, declarators: List[IRVariableDeclarator]) {
  override def toString: String = localType.name + ' ' + declarators.mkString(",")
}

case class IRVariableDeclarator (name: String, dim: Int, init: Option[IRExpression]) {
  private def typeName = name + (0 until dim).map(_ => "[]").mkString
  override def toString: String = typeName + init.map(e => " " + e.toString).getOrElse("")
}

case class IRIfStatement (condition: IRExpression, thenStatement: IRStatement, elseStatement: Option[IRStatement]) extends IRStatement

case class IRWhileStatement (condition: IRExpression, statement: IRStatement) extends IRStatement

sealed trait IRForStatement extends IRStatement

case class IRNormalForStatement (local: IRLocalDeclaration, condition: Option[IRExpression], update: List[IRExpression], statement: IRStatement) extends IRForStatement
case class IRAncientForStatement (init: List[IRExpression], condition: Option[IRExpression], update: List[IRExpression], statement: IRStatement) extends IRForStatement
case class IREnhancedForStatement (elementType: JType, name: String, dim: Int, iterable: IRExpression, statement: IRStatement) extends IRForStatement

case class IRActivateStatement (expression: IRExpression) extends IRStatement

case class IRThrowStatement (expression: IRExpression) extends IRStatement

case class IRReturnStatement (expression: IRExpression) extends IRStatement

case class IRExpressionStatement (expression: IRExpression) extends IRStatement {
  def activates: List[IRContextRef] = expression.activates
  def deactivates: List[IRContextRef] = expression.deactivates
}