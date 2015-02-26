package phenan.prj.ir

import phenan.prj._
import phenan.prj.decl._

import JModifier._

sealed trait IRModule extends JClass {
  protected val declaration: ModuleDeclaration

  def name: String = file.getPackageName match {
    case Some(pack) => pack + "." + declaration.name
    case None       => declaration.name
  }

  val internalName: String = file.getPackageInternalName match {
    case Some(pack) => pack + "/" + declaration.name
    case None       => declaration.name
  }

  def file: IRFile

  protected def getModifierFlag (modifier: Modifier): Int = modifier match {
    case PublicModifier       => accPublic
    case PrivateModifier      => accPrivate
    case ProtectedModifier    => accProtected
    case StaticModifier       => accStatic
    case FinalModifier        => accFinal
    case SynchronizedModifier => accSynchronized
    case VolatileModifier     => accVolatile
    case TransientModifier    => accTransient
    case NativeModifier       => accNative
    case AbstractModifier     => accAbstract
    case StrictFPModifier     => accStrict
    case _ => 0
  }
}

class IRClass (protected val declaration: ClassDeclaration, val file: IRFile) extends IRModule with TypeNameResolver {
  val mod: JModifier = JModifier(declaration.modifiers.foldRight(accSuper)((mod, flags) => flags | getModifierFlag(mod)))

  lazy val typeVariables = declaration.typeParameters.foldLeft (Map.empty[String, IRTypeVariable]) { (env, param) =>
    env + (param.name -> IRTypeParameter(param, env, file.resolver).typeVariable)
  }

  lazy val typeParameters = declaration.typeParameters.map(param => typeVariables(param.name).parameter)

  lazy val superType: Option[IRGenericClassType] = declaration.superClass.flatMap(classTypeName)

  lazy val interfaceTypes: List[IRGenericClassType] = declaration.interfaces.flatMap(classTypeName)

  lazy val members: List[IRClassMemberDef] = ???

  override def superClass: Option[JClass] = superType.flatMap(_.erase)
  override def interfaces: List[JClass] = interfaceTypes.flatMap(_.erase)

  override def outerClass: Option[JClass] = None
  override def innerClasses: Map[String, JClass] = ???

  override def methods: List[JMethodDef] = ???

  override def fields: List[JFieldDef] = ???

  override def classType: JClassType = ???
  override def objectType(typeArgs: List[JValueType]): Option[JObjectType] = ???

  protected def resolver = file.resolver
}

sealed trait IRClassMemberDef
