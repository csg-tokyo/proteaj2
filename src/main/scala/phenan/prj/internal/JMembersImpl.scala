package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception._
import phenan.prj.state.JState

class JFieldImpl (fieldDef: JFieldDef, val fieldType: JType, val declaring: JObjectTypeImpl) extends JField {
  override def modifier: JModifier = fieldDef.mod
  override def name: String = fieldDef.name
}

class JMethodImpl (val methodDef: JMethodDef, val env: Map[String, MetaValue], val declaring: JObjectTypeImpl)(implicit val state: JState) extends JMethod {
  def modifier: JModifier = methodDef.mod
  def name: String = methodDef.name

  def erasedReturnType: JErasedType = methodDef.returnType
  def erasedParameterTypes: List[JErasedType] = methodDef.paramTypes

  override def returnType: JGenericType = ???

  override def parameterTypes: List[JGenericType] = ???
  override def exceptionTypes: List[JGenericType] = ???
}

class JConstructorImpl (val methodDef: JMethodDef, val enclosingEnv: Map[String, MetaValue], val declaring: JObjectTypeImpl)(implicit val state: JState) extends JConstructor {
  override def modifier: JModifier = methodDef.mod
}

class JGenericTypeImpl (val signature: JTypeSignature, val env: Map[String, MetaValue], loader: JClassLoaderImpl)(implicit val state: JState) extends JGenericType {
  override def bind(args: Map[String, MetaValue]): Option[JType] = ???

  override def >=> (t: JType): Option[Map[String, MetaValue]] = ???

  override def <=< (t: JType): Option[Map[String, MetaValue]] = ???

  override def <=> (t: JType): Option[Map[String, MetaValue]] = ???
}
