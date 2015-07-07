package phenan.prj.ir

import phenan.prj._

import scala.util._

sealed trait IRExpression {
  def staticType: Option[JType] = ???
  def activates: List[IRContextRef] = ???
  def deactivates: List[IRContextRef] = ???
}

sealed trait IRLeftHandSide extends IRExpression

case class IRDSLOperation (method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExpression

case class IRContextOperation (context: IRContextRef, method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExpression

sealed trait IRAssignmentExpression extends IRExpression {
  def left: IRLeftHandSide
  def right: IRExpression
}

case class IRSimpleAssignmentExpression (left: IRLeftHandSide, right: IRExpression) extends IRAssignmentExpression

case class IRNewExpression (metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExpression

sealed trait IRArrayCreation extends IRExpression

case class IRNewArray (componentType: JType, length: List[IRExpression], dim: Int) extends IRArrayCreation

case class IRArrayInitializer (componentType: JType, dim: Int, components: List[IRExpression]) extends IRArrayCreation

case class IRCastExpression (destType: JType, expression: IRExpression) extends IRExpression

case class IRArrayAccess (array: IRExpression, index: IRExpression) extends IRLeftHandSide

sealed trait IRFieldAccess extends IRLeftHandSide

case class IRInstanceFieldAccess (instance: IRExpression, field: JField) extends IRFieldAccess

case class IRSuperFieldAccess (superType: JObjectType, field: JField) extends IRFieldAccess

case class IRStaticFieldAccess (field: JField) extends IRFieldAccess

case class IRStaticMethodCall (method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression]) extends IRExpression

case class IRVariableArguments (args: List[IRExpression]) extends IRExpression

sealed trait IRClassLiteral extends IRExpression

case class IRObjectClassLiteral (clazz: JClass, dim: Int) extends IRClassLiteral

case class IRPrimitiveClassLiteral (primitiveClass: JPrimitiveType, dim: Int) extends IRClassLiteral

case class IRThisRef (thisType: JObjectType) extends IRExpression

case class IRLocalVariableRef (localType: JType, name: String) extends IRLeftHandSide

case class IRContextRef (contextType: JObjectType, id: Int) extends IRExpression
