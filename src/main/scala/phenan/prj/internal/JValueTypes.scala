package phenan.prj.internal

import phenan.prj._

import scala.util.Try

private[internal] trait JType_Internal {
  def loader: JClassLoader
}

class JLoadedObjectType (val erase: JLoadedClass, val typeArguments: Map[String, JValueType]) extends JObjectType with JType_Internal {
  override def name: String = {
    if (typeArguments.isEmpty) erase.name
    else erase.name + typeArguments.map(kv => kv._1 + "=" + kv._2.name).mkString("<", ",", ">")
  }

  lazy val superType: Option[JObjectType] = erase.signature match {
    case Some(sig) => JTypePool.fromClassTypeSignature(sig.superClass, typeArguments, loader)
    case None      => erase.superClass.flatMap(_.objectType(Nil))
  }

  lazy val interfaceTypes: List[JObjectType] = erase.signature match {
    case Some(sig) => sig.interfaces.flatMap(JTypePool.fromClassTypeSignature(_, typeArguments, loader))
    case None      => erase.interfaces.flatMap(_.objectType(Nil))
  }

  override def constructors: List[JConstructor] = ???

  override def declaredFields: List[JField] = ???

  override def declaredMethods: List[JMethod] = ???

  override def isAssignableTo(that: JValueType): Boolean = ???

  override def array: JArrayType = JTypePool.arrayOf(this)

  override def loader: JClassLoader = erase.loader
}

class JArrayTypeImpl (val componentType: JValueType with JType_Internal) extends JArrayType with JType_Internal {

  lazy val name: String = componentType.name + "[]"

  override def superTypes: List[JObjectType] = JTypePool.superTypesOfArray(loader)

  override def fields: Map[String, JField] = ???
  override def methods: Map[String, List[JMethod]] = ???

  override def array: JArrayType = JTypePool.arrayOf(this)

  override def loader: JClassLoader = componentType.loader
}

class JPrimitiveTypeImpl (clazz: JPrimitiveClassImpl) extends JPrimitiveType with JType_Internal {

  override def name: String = clazz.name

  lazy val wrapperType: Option[JValueType] = clazz.wrapperClass.flatMap(_.objectType(Nil))

  override def isAssignableTo(that: JValueType): Boolean = ???

  override def array: JArrayType = JTypePool.arrayOf(this)

  override def loader: JClassLoader = clazz.loader
}

class JWildcardTypeImpl (val upperBound: JValueType, val lowerBound: Option[JValueType], val loader: JClassLoader) extends JWildcardType with JType_Internal {
  override def name: String = ???

  override def array: JArrayType = JTypePool.arrayOf(this)

  override def methods: Map[String, List[JMethod]] = ???

  override def fields: Map[String, JField] = ???
}