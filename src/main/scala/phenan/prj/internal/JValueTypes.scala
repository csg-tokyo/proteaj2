package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

class JObjectTypeImpl (val erase: JClass, val env: Map[String, MetaValue])(implicit state: JState) extends JObjectType {
  override def name: String = {
    if (env.isEmpty) erase.name
    else erase.name + env.map(kv => kv._1 + "=" + kv._2).mkString("<", ",", ">")
  }

  lazy val superType: Option[JObjectType] = erase.signature match {
    case Some(sig) => compiler.typeLoader.fromClassTypeSignature(sig.superClass, env)
    case None      => erase.superClass.flatMap(_.objectType(Nil))
  }

  lazy val interfaceTypes: List[JObjectType] = erase.signature match {
    case Some(sig) => sig.interfaces.flatMap(compiler.typeLoader.fromClassTypeSignature(_, env))
    case None      => erase.interfaces.flatMap(_.objectType(Nil))
  }

  lazy val constructors: List[JConstructor] = {
    erase.methods.filter(_.isConstructor).map { constructorDef => new JConstructorImpl(constructorDef, env, this) }
  }

  lazy val declaredFields: List[JField] = {
    erase.fields.filterNot(_.isStatic).flatMap { fieldDef =>
      fieldDef.signature.flatMap(sig => compiler.typeLoader.fromTypeSignature(sig, env)).map(fieldType => new JFieldImpl(fieldDef, fieldType, this))
    }
  }

  lazy val declaredMethods: List[JMethod] = {
    erase.methods.filter(_.isInstanceMethod).map { methodDef => new JMethodImpl(methodDef, env, this) }
  }

  override def isAssignableTo(that: JType): Boolean = ???

  override def array: JArrayType = compiler.typeLoader.arrayOf(this)

  def compiler = erase.compiler
}

class JArrayTypeImpl (val componentType: JType)(implicit state: JState) extends JArrayType {

  lazy val name: String = componentType.name + "[]"

  override def superTypes: List[JObjectType] = compiler.typeLoader.superTypesOfArray

  override def fields: Map[String, JField] = ???
  override def methods: Map[String, List[JMethod]] = ???

  override def array: JArrayType = compiler.typeLoader.arrayOf(this)

  def compiler = componentType.compiler
}

class JPrimitiveTypeImpl (clazz: JPrimitiveClassImpl)(implicit state: JState) extends JPrimitiveType {

  override def name: String = clazz.name

  lazy val wrapperType: Option[JType] = clazz.wrapperClass.flatMap(_.objectType(Nil))

  override def isAssignableTo(that: JType): Boolean = ???

  override def array: JArrayType = compiler.typeLoader.arrayOf(this)

  val compiler = clazz.compiler
}

class JWildcardTypeImpl (val upperBound: JType, val lowerBound: Option[JType], val compiler: JCompiler)(implicit state: JState) extends JWildcardType {
  override def name: String = ???

  override def array: JArrayType = compiler.typeLoader.arrayOf(this)

  override def methods: Map[String, List[JMethod]] = ???

  override def fields: Map[String, JField] = ???

}