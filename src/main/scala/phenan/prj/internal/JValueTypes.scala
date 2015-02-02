package phenan.prj.internal

import phenan.prj._

class JLoadedObjectType (val erase: JLoadedClass, val typeArguments: Map[String, JValueType]) extends JObjectType {
  override def name: String = {
    if (typeArguments.isEmpty) erase.name
    else erase.name + typeArguments.map(kv => kv._1 + "=" + kv._2.name).mkString("<", ",", ">")
  }

  override def constructors: List[JConstructor] = ???

  override def declaredFields: List[JField] = ???

  override def interfaceTypes: List[JObjectType] = ???

  override def superType: Option[JObjectType] = ???

  override def declaredMethods: List[JMethod] = ???

  override def isAssignableTo(that: JValueType): Boolean = ???

  override def array: JArrayType = JTypePool.arrayOf(this)
}

class JArrayTypeImpl (val componentType: JValueType) extends JArrayType {

  lazy val name: String = componentType.name + "[]"

  override def superType: JObjectType = ???
  override def interfaceTypes: List[JObjectType] = ???

  override def fields: Map[String, JField] = ???
  override def methods: Map[String, List[JMethod]] = ???

  override def array: JArrayType = JTypePool.arrayOf(this)
}

class JPrimitiveTypeImpl (clazz: JPrimitiveClass) extends JPrimitiveType {

  override def name: String = clazz.name

  override def wrapperType: JValueType = clazz.wrapperClass.objectType(Nil).get

  override def isAssignableTo(that: JValueType): Boolean = ???

  override def array: JArrayType = JTypePool.arrayOf(this)
}

class JWildcardTypeImpl (val upperBound: JValueType, val lowerBound: Option[JValueType]) extends JWildcardType {
  override def name: String = ???

  override def array: JArrayType = JTypePool.arrayOf(this)

  override def methods: Map[String, List[JMethod]] = ???

  override def fields: Map[String, JField] = ???
}