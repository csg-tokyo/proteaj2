package phenan.prj

import CommonNames._

sealed trait JErasedType {
  def name: String
  def isSubclassOf (that: JErasedType): Boolean
  def compiler: JCompiler
}

trait JClass extends JErasedType {
  def mod: JModifier

  def fields: List[JFieldDef]
  def methods: List[JMethodDef]

  def internalName: String
  def innerClasses: Map[String, String]
  def outerClass: Option[String]

  def signature: JClassSignature

  lazy val superClass: Option[JClass] = compiler.classLoader.erase_PE(signature.superClass)
  lazy val interfaces: List[JClass] = signature.interfaces.flatMap(compiler.classLoader.erase_PE)

  def isSubclassOf (that: JErasedType): Boolean = {
    this == that || superClass.exists(_.isSubclassOf(that)) || interfaces.exists(_.isSubclassOf(that))
  }

  def classModule: JClassModule = JClassModule(this)
  def objectType (typeArgs: List[MetaValue]): Option[JObjectType] = compiler.typeLoader.getObjectType(this, typeArgs)

  lazy val classInitializer = methods.find(_.isClassInitializer)
  lazy val constructors     = methods.filter(_.isConstructor)
  lazy val instanceMethods  = methods.filter(_.isInstanceMethod)
  lazy val staticMethods    = methods.filter(_.isStaticMethod)
  lazy val instanceFields   = fields.filterNot(_.isStatic)
  lazy val staticFields     = fields.filter(_.isStatic)
}

trait JPrimitiveClass extends JErasedType {
  def wrapperClass: Option[JClass]
  def isSubclassOf (that: JErasedType): Boolean = this == that
  lazy val primitiveType: JPrimitiveType = JPrimitiveType(this)
}

trait JArrayClass extends JErasedType {
  def component: JErasedType
  def isSubclassOf (that: JErasedType): Boolean = that match {
    case that: JArrayClass  => this == that || this.component.isSubclassOf(that.component)
    case that: JClass       => superClassesOfArray.contains(that.internalName)
    case _: JPrimitiveClass => false
  }
}

trait JFieldDef {
  def mod: JModifier
  def name: String
  def declaringClass: JClass

  def signature: JTypeSignature

  def isStatic: Boolean = mod.check(JModifier.accStatic)
}

trait JMethodDef {
  def mod: JModifier
  def name: String
  def declaringClass: JClass

  def signature: JMethodSignature

  def erasedReturnType: JErasedType
  def erasedParameterTypes: List[JErasedType]

  def isStatic: Boolean           = mod.check(JModifier.accStatic)
  def isConstructor: Boolean      = name == constructorName
  def isClassInitializer: Boolean = name == classInitializerName
  def isInstanceMethod: Boolean   = ! (isStatic || isConstructor || isClassInitializer)
  def isStaticMethod: Boolean     = isStatic && ! (isConstructor || isClassInitializer)
}
