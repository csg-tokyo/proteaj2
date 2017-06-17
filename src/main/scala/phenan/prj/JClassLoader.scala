package phenan.prj

import phenan.prj.exception._

import phenan.prj.CommonNames._
import phenan.prj.signature.DescriptorParser

import scala.util._
import scalaz.Memo.mutableHashMapMemo

trait JClassLoader {
  this: JCompiler with DescriptorParser with JErasedTypes with Application =>

  def loadClass: (String) => Try[JClass]

  def load (name: String): Try[JErasedType] = {
    if (name.startsWith("[")) parseArrayDescriptor(name) match {
      case Some(desc) => erase(desc, Nil)
      case None => Failure(InvalidTypeException("invalid type descriptor : " + name))
    }
    else loadClass(name)
  }

  def load (name: String, dim: Int): Try[JErasedType] = {
    if (dim > 0) loadClass(name).map(clazz => arrayOf(clazz, dim))
    else loadClass(name)
  }

  def loadClass_PE (name: String): Option[JClass] = loadClass(name) match {
    case Success(clazz) => Some(clazz)
    case Failure(e)     =>
      error("fail to load class " + name, e)
      None
  }

  def loadInnerClass(outer: JClass, name: String): Try[JClass] = outer.innerClasses.get(name).map(loadClass) getOrElse {
    Failure(ClassFileNotFoundException("class " + outer.name + "$" + name + " is not found"))
  }

  def arrayClassOf: JErasedType => JArrayClass = mutableHashMapMemo(JArrayClass)

  def arrayOf(clazz: JErasedType, dim: Int): JErasedType = {
    if (dim > 0) arrayOf(arrayClassOf(clazz), dim - 1)
    else clazz
  }

  def booleanClass: JPrimitiveClass = JPrimitiveClass("boolean", boxedBooleanClassName)
  def byteClass: JPrimitiveClass    = JPrimitiveClass("byte", boxedByteClassName)
  def charClass: JPrimitiveClass    = JPrimitiveClass("char", boxedCharClassName)
  def shortClass: JPrimitiveClass   = JPrimitiveClass("short", boxedShortClassName)
  def intClass: JPrimitiveClass     = JPrimitiveClass("int", boxedIntClassName)
  def longClass: JPrimitiveClass    = JPrimitiveClass("long", boxedLongClassName)
  def floatClass: JPrimitiveClass   = JPrimitiveClass("float", boxedFloatClassName)
  def doubleClass: JPrimitiveClass  = JPrimitiveClass("double", boxedDoubleClassName)
  def voidClass: JPrimitiveClass    = JPrimitiveClass("void", boxedVoidClassName)

  lazy val primitives: Map[String, JPrimitiveClass] = Map(
    "boolean" -> booleanClass, "byte" -> byteClass, "char" -> charClass, "short" -> shortClass, "int" -> intClass,
    "long" -> longClass, "float" -> floatClass, "double" -> doubleClass, "void" -> voidClass
  )

  lazy val objectClass: Option[JClass] = loadClass_PE(CommonNames.objectClassName)
  lazy val stringClass: Option[JClass] = loadClass_PE(CommonNames.stringClassName)
  lazy val classClass: Option[JClass] = loadClass_PE(CommonNames.classClassName)
  lazy val iterableClass: Option[JClass] = loadClass_PE(CommonNames.iterableClassName)
  lazy val errorClass: Option[JClass] = loadClass_PE(CommonNames.errorClassName)
  lazy val runtimeExceptionClass: Option[JClass] = loadClass_PE(CommonNames.runtimeExceptionName)
  lazy val functionClass: Option[JClass] = loadClass_PE(CommonNames.functionClassName)

  lazy val predefOperatorsClass: Option[JClass] = loadClass_PE(CommonNames.predefOperatorsName)
  lazy val typeClass: JClass = JTypeClass

  def erase (signature: JTypeSignature, metaParams: List[FormalMetaParameter]): Try[JErasedType] = signature match {
    case prm: JPrimitiveTypeSignature => Success(erase(prm))
    case arr: JArrayTypeSignature     => erase(arr, metaParams)
    case cls: JClassTypeSignature     => erase(cls)
    case cap: JCapturedWildcardSignature => cap.upperBound.map(erase(_, metaParams)).getOrElse(loadClass(CommonNames.objectClassName))
    case tvr: JTypeVariableSignature     => metaParams.find(_.name == tvr.name).flatMap(_.bounds.headOption).map(erase(_, metaParams)).getOrElse(loadClass(CommonNames.objectClassName))
  }

  def erase (signature: JPrimitiveTypeSignature): JPrimitiveClass = signature match {
    case ByteTypeSignature   => byteClass
    case CharTypeSignature   => charClass
    case DoubleTypeSignature => doubleClass
    case FloatTypeSignature  => floatClass
    case IntTypeSignature    => intClass
    case LongTypeSignature   => longClass
    case ShortTypeSignature  => shortClass
    case BoolTypeSignature   => booleanClass
    case VoidTypeSignature   => voidClass
  }

  def erase (signature: JArrayTypeSignature, metaParams: List[FormalMetaParameter]): Try[JArrayClass] = erase(signature.component, metaParams).map(arrayClassOf)

  def erase (signature: JClassTypeSignature): Try[JClass] = signature match {
    case SimpleClassTypeSignature(clazz, args)        => loadClass(clazz)
    case MemberClassTypeSignature(outer, clazz, args) => erase(outer).flatMap(loadInnerClass(_, clazz))
  }

  def erase_PE (signature: JClassTypeSignature): Option[JClass] = erase(signature) match {
    case Success(t) => Some(t)
    case Failure(e) =>
      error("class type not found : " + signature, e)
      None
  }

  def erase_Force (signature: JTypeSignature, metaParams: List[FormalMetaParameter]): JErasedType = erase(signature, metaParams) match {
    case Success(t) => t
    case Failure(e) =>
      error("type not found : " + signature, e)
      objectClass.get
  }
}
