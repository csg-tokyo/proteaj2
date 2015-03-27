package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

private[internal] trait JType_Internal {
  def loader: JClassLoader
}

class JLoadedObjectType (val erase: JLoadedClass, val env: Map[String, MetaValue])(implicit state: JState) extends JObjectType with JType_Internal {
  override def name: String = {
    if (env.isEmpty) erase.name
    else erase.name + env.map(kv => kv._1 + "=" + kv._2).mkString("<", ",", ">")
  }

  lazy val superType: Option[JObjectType] = erase.signature match {
    case Some(sig) => typePool.fromClassTypeSignature(sig.superClass, env, loader)
    case None      => erase.superClass.flatMap(_.objectType(Nil))
  }

  lazy val interfaceTypes: List[JObjectType] = erase.signature match {
    case Some(sig) => sig.interfaces.flatMap(typePool.fromClassTypeSignature(_, env, loader))
    case None      => erase.interfaces.flatMap(_.objectType(Nil))
  }

  lazy val constructors: List[JGenConstructor] = {
    erase.methods.filter(_.isConstructor).map { constructorDef => new JGenLoadedConstructor(constructorDef, env, this) }
  }

  lazy val declaredFields: List[JField] = {
    erase.fields.filterNot(_.isStatic).flatMap { fieldDef =>
      fieldDef.signature.flatMap(sig => typePool.fromTypeSignature(sig, env, loader)).map(fieldType => new JLoadedField(fieldDef, fieldType, this))
    }
  }

  lazy val declaredMethods: List[JGenMethod] = {
    erase.methods.filter(_.isInstanceMethod).map { methodDef => new JGenLoadedInstanceMethod(methodDef, env, this) }
  }

  override def isAssignableTo(that: JType): Boolean = ???

  override def array: JArrayType = typePool.arrayOf(this)

  override def loader: JClassLoader = erase.loader

  private def typePool = JTypePool.get
}

class JArrayTypeImpl (val componentType: JType with JType_Internal)(implicit state: JState) extends JArrayType with JType_Internal {

  lazy val name: String = componentType.name + "[]"

  override def superTypes: List[JObjectType] = typePool.superTypesOfArray(loader)

  override def fields: Map[String, JField] = ???
  override def methods: Map[String, List[JGenMethod]] = ???

  override def array: JArrayType = typePool.arrayOf(this)

  override def loader: JClassLoader = componentType.loader

  private def typePool = JTypePool.get
}

class JPrimitiveTypeImpl (clazz: JPrimitiveClassImpl)(implicit state: JState) extends JPrimitiveType with JType_Internal {

  override def name: String = clazz.name

  lazy val wrapperType: Option[JType] = clazz.wrapperClass.flatMap(_.objectType(Nil))

  override def isAssignableTo(that: JType): Boolean = ???

  override def array: JArrayType = typePool.arrayOf(this)

  override def loader: JClassLoader = clazz.loader

  private def typePool = JTypePool.get
}

class JWildcardTypeImpl (val upperBound: JType, val lowerBound: Option[JType], val loader: JClassLoader)(implicit state: JState) extends JWildcardType with JType_Internal {
  override def name: String = ???

  override def array: JArrayType = typePool.arrayOf(this)

  override def methods: Map[String, List[JGenMethod]] = ???

  override def fields: Map[String, JField] = ???

  private def typePool = JTypePool.get
}