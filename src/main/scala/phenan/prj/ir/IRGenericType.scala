package phenan.prj.ir

import phenan.prj._
import phenan.prj.decl._
import phenan.prj.state.JState

import scala.util._

case class IRTypeParameter (typeParameter: TypeParameter, typeVariables: Map[String, IRTypeVariable], resolver: IRResolver)(implicit state: JState) {
  def name = typeParameter.name

  lazy val bounds: List[IRGenericClassType] = typeParameter.bounds.flatMap { bound =>
    resolver.classTypeName(bound, typeVariables) match {
      case Success(t) => Some(t)
      case Failure(e) =>
        state.error("type " + bound + " is not found", e)
        None
    }
  }

  val typeVariable: IRTypeVariable = new IRTypeVariable(this, resolver)
}

sealed trait IRTypeArgument

sealed trait IRGenericType extends IRTypeArgument {
  def erase: JErasedType
}

sealed trait IRGenericRefType extends IRGenericType {
  def erase: JClass
}

case class IRGenericClassType (clazz: JClass, args: List[IRTypeArgument]) extends IRGenericRefType {
  def erase: JClass = clazz
}

class IRTypeVariable (val parameter: IRTypeParameter, resolver: IRResolver) extends IRGenericRefType {
  def name: String = parameter.name
  lazy val erase: JClass = parameter.bounds.headOption.map(_.erase).getOrElse(resolver.objectClass)
}

case class IRGenericArrayType (component: IRGenericType, resolver: IRResolver) extends IRGenericType {
  lazy val erase: JArrayClass = resolver.arrayOf(component.erase)
}

case class IRGenericPrimitiveType (primitiveClass: JPrimitiveClass) extends IRGenericType {
  def erase: JPrimitiveClass = primitiveClass
}

case class IRGenericWildcardType (upper: Option[IRGenericType], lower: Option[IRGenericType]) extends IRTypeArgument

