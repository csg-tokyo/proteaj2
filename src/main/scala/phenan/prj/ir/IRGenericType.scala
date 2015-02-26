package phenan.prj.ir

import phenan.prj._
import phenan.prj.decl._

case class IRTypeParameter (typeParameter: TypeParameter, typeVariables: Map[String, IRTypeVariable], resolver: IRResolver) extends TypeNameResolver {
  def name = typeParameter.name

  lazy val bounds: List[IRGenericClassType] = typeParameter.bounds.flatMap(classTypeName)

  val typeVariable: IRTypeVariable = new IRTypeVariable(this, resolver)
}

sealed trait IRTypeArgument

sealed trait IRGenericType extends IRTypeArgument {
  def erase: Option[JErasedType]
}

case class IRGenericClassType (clazz: JClass, args: List[IRTypeArgument]) extends IRGenericType {
  def erase: Option[JClass] = Some(clazz)
}

class IRTypeVariable (val parameter: IRTypeParameter, resolver: IRResolver) extends IRGenericType {
  def name: String = parameter.name
  lazy val erase: Option[JClass] = parameter.bounds.headOption.flatMap(_.erase).orElse(resolver.objectClass)
}

case class IRGenericArrayType (component: IRGenericType, resolver: IRResolver) extends IRGenericType {
  lazy val erase: Option[JArrayClass] = component.erase.map(resolver.arrayOf)
}

case class IRGenericPrimitiveType (primitiveClass: JPrimitiveClass) extends IRGenericType {
  def erase: Option[JPrimitiveClass] = Some(primitiveClass)
}

case class IRGenericWildcardType (upper: Option[IRGenericType], lower: Option[IRGenericType]) extends IRTypeArgument

