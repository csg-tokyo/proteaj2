package phenan.jir

import scala.util.Try

sealed trait JType {
  def fields: Map[String, JField]
  def methods: Map[String, List[JMethod]]

  def isSubtypeOf (that: JType): Boolean
  def isAssignableTo (that: JType): Boolean
}

trait JClassType extends JType {
  def isSubtypeOf (that: JType): Boolean = this == that
  def isAssignableTo (that: JType): Boolean = false
}

trait JObjectType extends JType {
  def bind (args: List[JType]): Try[JObjectType]
  def typeArguments: List[JType]

  def erase: JClass
  def superType: Option[JType]
  def interfaceTypes: List[JType]

  def declaredFields: Map[String, JField]
  def declaredMethods: Map[String, JMethod]

  def constructors: List[JConstructor]

  def isSubtypeOf (that: JType): Boolean = that match {
    case that: JObjectType   => isSubtypeOf(that)
    case that: JWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _                   => false
  }

  def isSubtypeOf (that: JObjectType): Boolean = {
    isMatchedTo(that) || superType.exists(_.isSubtypeOf(that)) || interfaceTypes.exists(_.isSubtypeOf(that))
  }

  def isMatchedTo (that: JObjectType): Boolean = this == that
}

trait JPrimitiveType extends JType {
  def wrapperType: JObjectType

  def fields: Map[String, JField] = Map.empty
  def methods: Map[String, List[JMethod]] = Map.empty
}

trait JArrayType extends JType {
  def componentType: JType
}

trait JWildcardType extends JType {
  def upperBound: JType
  def lowerBound: Option[JType]
}
