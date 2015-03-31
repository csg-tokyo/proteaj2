package phenan.prj

import CommonNames._

sealed trait JErasedType {
  def name: String
  def isSubclassOf (that: JErasedType): Boolean
  def compiler: JCompiler
}

trait JClass extends JErasedType {
  def mod: JModifier
  def superClass: Option[JClass]
  def interfaces: List[JClass]
  def innerClasses: Map[String, JClass]
  def outerClass: Option[JClass]
  def fields: List[JFieldDef]
  def methods: List[JMethodDef]

  def signature: Option[JClassSignature]

  def isSubclassOf (that: JErasedType): Boolean = {
    this == that || superClass.exists(_.isSubclassOf(that)) || interfaces.exists(_.isSubclassOf(that))
  }

  def classModule: JClassModule
  def objectType (typeArgs: List[MetaValue]): Option[JObjectType]

  lazy val classInitializer = methods.find(_.isClassInitializer)
  lazy val constructors     = methods.filter(_.isConstructor)
  lazy val instanceMethods  = methods.filter(_.isInstanceMethod)
  lazy val staticMethods    = methods.filter(_.isStaticMethod)
  lazy val instanceFields   = fields.filterNot(_.isStatic)
  lazy val staticFields     = fields.filter(_.isStatic)
}

trait JPrimitiveClass extends JErasedType {
  def primitiveType: JPrimitiveType
  def isSubclassOf (that: JErasedType): Boolean = this == that
}

trait JArrayClass extends JErasedType {
  def component: JErasedType
  def isSubclassOf (that: JErasedType): Boolean = that match {
    case that: JArrayClass => this == that || this.component.isSubclassOf(that.component)
    case _ => superClassesOfArray.contains(that.name)
  }
}

trait JFieldDef {
  def mod: JModifier
  def name: String
  def fieldType: JErasedType
  def declaringClass: JClass

  def signature: Option[JTypeSignature]

  def isStatic: Boolean = mod.check(JModifier.accStatic)
}

trait JMethodDef {
  def mod: JModifier
  def name: String
  def returnType: JErasedType
  def paramTypes: List[JErasedType]
  def exceptions: List[JClass]
  def declaringClass: JClass

  def isStatic: Boolean           = mod.check(JModifier.accStatic)
  def isConstructor: Boolean      = name == constructorName
  def isClassInitializer: Boolean = name == classInitializerName
  def isInstanceMethod: Boolean   = ! (isStatic || isConstructor || isClassInitializer)
  def isStaticMethod: Boolean     = isStatic && ! (isConstructor || isClassInitializer)
}
