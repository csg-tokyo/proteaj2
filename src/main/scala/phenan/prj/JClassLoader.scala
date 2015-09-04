package phenan.prj

import phenan.prj.exception.ClassFileNotFoundException
import phenan.prj.state.JState

import scala.util._

trait JClassLoader {
  def load(name: String): Try[JErasedType]
  def load(name: String, dim: Int): Try[JErasedType]
  def loadClass: String => Try[JClass]

  def loadClass_PE (name: String): Option[JClass] = loadClass(name) match {
    case Success(clazz) => Some(clazz)
    case Failure(e)     =>
      state.error("fail to load class " + name, e)
      None
  }

  def loadInnerClass(outer: JClass, name: String): Try[JClass] = outer.innerClasses.get(name).map(loadClass) getOrElse {
    Failure(ClassFileNotFoundException("class " + outer.name + "$" + name + " is not found"))
  }

  def arrayOf: JErasedType => JArrayClass
  def arrayOf(clazz: JErasedType, dim: Int): JErasedType

  def boolean: JPrimitiveClass
  def byte: JPrimitiveClass
  def char: JPrimitiveClass
  def short: JPrimitiveClass
  def int: JPrimitiveClass
  def long: JPrimitiveClass
  def float: JPrimitiveClass
  def double: JPrimitiveClass
  def void: JPrimitiveClass

  lazy val primitives: Map[String, JPrimitiveClass] = Map(
    "boolean" -> boolean, "byte" -> byte, "char" -> char, "short" -> short, "int" -> int,
    "long" -> long, "float" -> float, "double" -> double, "void" -> void
  )

  lazy val objectClass: Option[JClass] = loadClass_PE(CommonNames.objectClassName)
  lazy val stringClass: Option[JClass] = loadClass_PE(CommonNames.stringClassName)
  lazy val classClass: Option[JClass] = loadClass_PE(CommonNames.classClassName)
  lazy val iterableClass: Option[JClass] = loadClass_PE(CommonNames.iterableClassName)
  lazy val errorClass: Option[JClass] = loadClass_PE(CommonNames.errorClassName)
  lazy val runtimeExceptionClass: Option[JClass] = loadClass_PE(CommonNames.runtimeExceptionName)
  lazy val functionClass: Option[JClass] = loadClass_PE(CommonNames.functionClassName)

  lazy val typeClass: JClass = new JTypeClass(compiler)

  def erase (signature: JTypeSignature, metaParams: List[FormalMetaParameter]): Try[JErasedType] = signature match {
    case prm: JPrimitiveTypeSignature => Success(erase(prm))
    case arr: JArrayTypeSignature     => erase(arr, metaParams)
    case cls: JClassTypeSignature     => erase(cls)
    case cap: JCapturedWildcardSignature => cap.upperBound.map(erase(_, metaParams)).getOrElse(loadClass(CommonNames.objectClassName))
    case tvr: JTypeVariableSignature     => metaParams.find(_.name == tvr.name).flatMap(_.bounds.headOption).map(erase(_, metaParams)).getOrElse(loadClass(CommonNames.objectClassName))
  }

  def erase (signature: JPrimitiveTypeSignature): JPrimitiveClass = signature match {
    case ByteTypeSignature   => byte
    case CharTypeSignature   => char
    case DoubleTypeSignature => double
    case FloatTypeSignature  => float
    case IntTypeSignature    => int
    case LongTypeSignature   => long
    case ShortTypeSignature  => short
    case BoolTypeSignature   => boolean
    case VoidTypeSignature   => void
  }

  def erase (signature: JArrayTypeSignature, metaParams: List[FormalMetaParameter]): Try[JArrayClass] = erase(signature.component, metaParams).map(arrayOf)

  def erase (signature: JClassTypeSignature): Try[JClass] = signature match {
    case SimpleClassTypeSignature(clazz, args)        => loadClass(clazz)
    case MemberClassTypeSignature(outer, clazz, args) => erase(outer).flatMap(loadInnerClass(_, clazz))
  }

  def erase_PE (signature: JClassTypeSignature): Option[JClass] = erase(signature) match {
    case Success(t) => Some(t)
    case Failure(e) =>
      state.error("class type not found : " + signature, e)
      None
  }

  def erase_Force (signature: JTypeSignature, metaParams: List[FormalMetaParameter]): JErasedType = erase(signature, metaParams) match {
    case Success(t) => t
    case Failure(e) =>
      state.error("type not found : " + signature, e)
      objectClass.get
  }

  def compiler: JCompiler
  implicit def state: JState = compiler.state
}
