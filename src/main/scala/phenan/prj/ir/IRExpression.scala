package phenan.prj.ir

import phenan.prj._

import scala.util._

sealed trait IRExpression {
  def staticType: Option[JType]
  def activates: List[IRContextRef] = ???
  def deactivates: List[IRContextRef] = ???
}

sealed trait IRLeftHandSide extends IRExpression

sealed trait IRAssignmentExpression extends IRExpression {
  def staticType: Option[JType] = right.staticType
  def left: IRLeftHandSide
  def right: IRExpression
}

case class IRSimpleAssignmentExpression (left: IRLeftHandSide, right: IRExpression) extends IRAssignmentExpression

case class IRNewExpression (metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExpression {
  def staticType = Some(constructor.declaring)
}

sealed trait IRArrayCreation extends IRExpression

case class IRNewArray (componentType: JType, length: List[IRExpression], dim: Int) extends IRArrayCreation {
  def staticType = Some(componentType.array(length.size + dim))
}

case class IRArrayInitializer (componentType: JType, dim: Int, components: List[IRExpression]) extends IRArrayCreation {
  def staticType = Some(componentType.array(dim))
}

case class IRCastExpression (destType: JType, expression: IRExpression) extends IRExpression {
  def staticType = Some(destType)
}

case class IRArrayAccess (array: IRExpression, index: IRExpression) extends IRLeftHandSide {
  def staticType = array.staticType match {
    case Some(JArrayType(component)) => Some(component)
    case _ => None
  }
}

sealed trait IRFieldAccess extends IRLeftHandSide {
  def field: JField
  def staticType = Some(field.fieldType)
}

case class IRInstanceFieldAccess (instance: IRExpression, field: JField) extends IRFieldAccess

case class IRSuperFieldAccess (superType: JObjectType, field: JField) extends IRFieldAccess

case class IRStaticFieldAccess (field: JField) extends IRFieldAccess

sealed trait IRMethodCall extends IRExpression {
  def metaArgs: Map[String, MetaArgument]
  def method: JMethod
  lazy val staticType: Option[JType] = method.returnType.bind(metaArgs)
}

case class IRInstanceMethodCall (instance: IRExpression, metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRSuperMethodCall (superType: JObjectType, metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRStaticMethodCall (metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRDSLOperation (method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRContextOperation (context: IRContextRef, method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRVariableArguments (args: List[IRExpression], staticType: Option[JType]) extends IRExpression

sealed trait IRClassLiteral extends IRExpression

case class IRObjectClassLiteral (clazz: JClass, dim: Int) extends IRClassLiteral {
  def staticType = clazz.objectType(Nil).map(_.array(dim)).flatMap(clazz.compiler.typeLoader.classTypeOf)
}

case class IRPrimitiveClassLiteral (primitiveClass: JPrimitiveType, dim: Int) extends IRClassLiteral {
  def staticType = primitiveClass.boxed.map(_.array(dim)).flatMap(primitiveClass.compiler.typeLoader.classTypeOf)
}

case class IRCharLiteral (value: Char, compiler: JCompiler) extends IRExpression {
  def staticType = Some(compiler.typeLoader.char)
}

case class IRIntLiteral (value: Int, compiler: JCompiler) extends IRExpression {
  def staticType = Some(compiler.typeLoader.int)
}

case class IRLongLiteral (value: Long, compiler: JCompiler) extends IRExpression {
  def staticType = Some(compiler.typeLoader.long)
}

case class IRBooleanLiteral (value: Boolean, compiler: JCompiler) extends IRExpression {
  def staticType = Some(compiler.typeLoader.boolean)
}

case class IRStringLiteral (value: String, compiler: JCompiler) extends IRExpression {
  def staticType = compiler.typeLoader.stringType
}

case class IRThisRef (thisType: JObjectType) extends IRExpression {
  def staticType = Some(thisType)
}

case class IRLocalVariableRef (localType: JType, name: String) extends IRLeftHandSide {
  def staticType = Some(localType)
}

case class IRContextRef (contextType: JObjectType, id: Int) extends IRExpression {
  def staticType = Some(contextType)
}
