package phenan.prj

import CommonNames._

sealed trait JErasedType {
  def name: String
  def isSubclassOf (that: JErasedType): Boolean
  def compiler: JCompiler
  implicit def state = compiler.state
}

trait JClass extends JErasedType {
  def mod: JModifier

  def fields: List[JFieldDef]
  def methods: List[JMethodDef]

  def internalName: String
  def innerClasses: Map[String, String]
  def outerClass: Option[String]

  def signature: JClassSignature

  def declaredPriorities: Set[JPriority]
  def memberPriorities: Set[JPriority]
  def priorityConstraints: List[List[JPriority]]
  def withDSLs: List[JClassTypeSignature]

  lazy val priorities: Set[JPriority] = declaredPriorities ++ memberPriorities

  lazy val superClass: Option[JClass] = compiler.classLoader.erase_PE(signature.superClass)
  lazy val interfaces: List[JClass] = signature.interfaces.flatMap(compiler.classLoader.erase_PE)

  def isSubclassOf (that: JErasedType): Boolean = {
    this == that || superClass.exists(_.isSubclassOf(that)) || interfaces.exists(_.isSubclassOf(that))
  }

  def classModule: JClassModule = JClassModule(this)
  def objectType (typeArgs: List[MetaArgument]): Option[JObjectType] = compiler.typeLoader.getObjectType(this, typeArgs)

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

class JTypeClass (val compiler: JCompiler) extends JClass {
  def mod: JModifier = JModifier(JModifier.accPublic | JModifier.accFinal | JModifier.accSuper)

  def internalName: String = CommonNames.typeClassName
  def name: String = internalName.replace('/', '.')

  def outerClass: Option[String] = None
  def innerClasses: Map[String, String] = Map.empty

  def methods: List[JMethodDef] = Nil
  def fields: List[JFieldDef] = Nil

  def withDSLs: List[JClassTypeSignature] = Nil
  def declaredPriorities: Set[JPriority] = Set.empty
  def memberPriorities: Set[JPriority] = Set.empty
  def priorityConstraints: List[List[JPriority]] = Nil

  def signature: JClassSignature = JClassSignature(Nil, JTypeSignature.objectTypeSig, Nil)
}

trait JFieldDef {
  def mod: JModifier
  def name: String
  def declaringClass: JClass

  def signature: JTypeSignature

  def isStatic: Boolean = mod.check(JModifier.accStatic)

  implicit def state = declaringClass.state
}

trait JMethodDef {
  def mod: JModifier
  def name: String
  def declaringClass: JClass

  def signature: JMethodSignature
  def syntax: Option[JSyntaxDef]

  lazy val erasedReturnType: JErasedType = compiler.classLoader.erase_Force(signature.returnType, signature.metaParams)
  lazy val erasedParameterTypes: List[JErasedType] = signature.parameters.map(param => compiler.classLoader.erase_Force(param.actualTypeSignature, signature.metaParams))

  def isStatic: Boolean              = mod.check(JModifier.accStatic)
  def isConstructor: Boolean         = name == constructorName
  def isClassInitializer: Boolean    = name == classInitializerName
  def isInstanceInitializer: Boolean = name == instanceInitializerName
  def isInstanceMethod: Boolean      = ! (isStatic || isConstructor || isClassInitializer || isInstanceInitializer)
  def isStaticMethod: Boolean        = isStatic && ! (isConstructor || isClassInitializer || isInstanceInitializer)

  def compiler = declaringClass.compiler
  implicit def state = declaringClass.state
}
