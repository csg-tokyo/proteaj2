package phenan.prj

import CommonNames._

trait JErasedTypes {
  this: JModules with JClassLoader with JTypeLoader =>

  sealed trait JErasedType {
    def name: String

    def isSubclassOf(that: JErasedType): Boolean
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

    def withDSLs: List[JClass]

    lazy val packageInternalName: String = internalName.substring(0, internalName.lastIndexOf('/'))

    lazy val priorities: Set[JPriority] = declaredPriorities ++ memberPriorities

    lazy val superClass: Option[JClass] = erase_PE(signature.superClass)
    lazy val interfaces: List[JClass] = signature.interfaces.flatMap(erase_PE)

    def isSubclassOf(that: JErasedType): Boolean = {
      this == that || superClass.exists(sup => sup != this && sup.isSubclassOf(that)) || interfaces.exists(_.isSubclassOf(that))
    }

    def classModule: JClassModule = JClassModule(this)

    def objectType (typeArgs: List[MetaArgument]): Option[JObjectType] = getObjectType(this, typeArgs)

    lazy val classInitializer: Option[JMethodDef] = methods.find(_.isClassInitializer)
    lazy val constructors: List[JMethodDef] = methods.filter(_.isConstructor)
    lazy val instanceMethods: List[JMethodDef] = methods.filter(_.isInstanceMethod)
    lazy val staticMethods: List[JMethodDef] = methods.filter(_.isStaticMethod)
    lazy val instanceFields: List[JFieldDef] = fields.filterNot(_.isStatic)
    lazy val staticFields: List[JFieldDef] = fields.filter(_.isStatic)

    def isClass: Boolean = !(mod.check(JModifier.accInterface) || mod.check(JModifier.accEnum))

    def isEnum: Boolean = mod.check(JModifier.accEnum)

    def isAnnotation: Boolean = mod.check(JModifier.accAnnotation)
  }

  case class JPrimitiveClass (name: String, wrapperName: String) extends JErasedType {
    def isSubclassOf(that: JErasedType): Boolean = this == that

    lazy val wrapperClass: Option[JClass] = loadClass_PE(wrapperName)
    lazy val primitiveType: JPrimitiveType = JPrimitiveType(this)
  }

  case class JArrayClass (component: JErasedType) extends JErasedType {
    lazy val name: String = component.name + "[]"

    def isSubclassOf(that: JErasedType): Boolean = that match {
      case that: JArrayClass => this == that || this.component.isSubclassOf(that.component)
      case that: JClass => superClassesOfArray.contains(that.internalName)
      case _: JPrimitiveClass => false
    }
  }

  object JTypeClass extends JClass {
    def mod: JModifier = JModifier(JModifier.accPublic | JModifier.accFinal | JModifier.accSuper)

    def internalName: String = CommonNames.typeClassName

    def name: String = internalName.replace('/', '.')

    def outerClass: Option[String] = None

    def innerClasses: Map[String, String] = Map.empty

    def methods: List[JMethodDef] = Nil

    def fields: List[JFieldDef] = Nil

    def withDSLs: List[JClass] = Nil

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
  }

  trait JMethodDef {
    def mod: JModifier

    def name: String

    def declaringClass: JClass

    def signature: JMethodSignature

    def syntax: Option[JSyntaxDef]

    lazy val erasedReturnType: JErasedType = erase_Force(signature.returnType, signature.metaParams)
    lazy val erasedParameterTypes: List[JErasedType] = signature.parameters.map(param => erase_Force(param.actualTypeSignature, signature.metaParams))

    def isStatic: Boolean = mod.check(JModifier.accStatic)

    def isConstructor: Boolean = name == constructorName

    def isClassInitializer: Boolean = name == classInitializerName

    def isInstanceInitializer: Boolean = name == instanceInitializerName

    def isInstanceMethod: Boolean = !(isStatic || isConstructor || isClassInitializer || isInstanceInitializer)

    def isStaticMethod: Boolean = isStatic && !(isConstructor || isClassInitializer || isInstanceInitializer)
  }
}
