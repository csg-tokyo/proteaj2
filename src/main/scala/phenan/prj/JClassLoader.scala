package phenan.prj

import phenan.prj.exception._

import phenan.prj.CommonNames._

import scala.util._
import scalaz.Memo.mutableHashMapMemo

trait JClassLoader {
  this: JCompiler with JErasedTypes with Application =>

  def loadClass: (String) => Try[JClass]

  def loadClass_NoFail (name: String): Option[JClass] = loadClass(name) match {
    case Success(clazz) => Some(clazz)
    case Failure(e)     =>
      error("fail to load class " + name, e)
      None
  }

  private def loadClass_Unsafe (name: String): JClass = loadClass(name).fold(e => throw InitializationFailedException(e), identity)

  def loadInnerClass(outer: JClass, name: String): Try[JClass] = outer.innerClasses.get(name).map(loadClass) getOrElse {
    Failure(ClassFileNotFoundException("class " + outer.name + "$" + name + " is not found"))
  }

  def arrayClassOf: JErasedType => JArrayClass = mutableHashMapMemo(JArrayClass)

  def arrayOf(clazz: JErasedType, dim: Int): JErasedType = {
    if (dim > 0) arrayOf(arrayClassOf(clazz), dim - 1)
    else clazz
  }

  lazy val booleanClass: JPrimitiveClass = JPrimitiveClass("boolean")
  lazy val byteClass: JPrimitiveClass    = JPrimitiveClass("byte")
  lazy val charClass: JPrimitiveClass    = JPrimitiveClass("char")
  lazy val shortClass: JPrimitiveClass   = JPrimitiveClass("short")
  lazy val intClass: JPrimitiveClass     = JPrimitiveClass("int")
  lazy val longClass: JPrimitiveClass    = JPrimitiveClass("long")
  lazy val floatClass: JPrimitiveClass   = JPrimitiveClass("float")
  lazy val doubleClass: JPrimitiveClass  = JPrimitiveClass("double")
  lazy val voidClass: JPrimitiveClass    = JPrimitiveClass("void")

  lazy val boxedBooleanClass: JClass = loadClass_Unsafe(boxedBooleanClassName)
  lazy val boxedByteClass: JClass    = loadClass_Unsafe(boxedByteClassName)
  lazy val boxedCharClass: JClass    = loadClass_Unsafe(boxedCharClassName)
  lazy val boxedShortClass: JClass   = loadClass_Unsafe(boxedShortClassName)
  lazy val boxedIntClass: JClass     = loadClass_Unsafe(boxedIntClassName)
  lazy val boxedLongClass: JClass    = loadClass_Unsafe(boxedLongClassName)
  lazy val boxedFloatClass: JClass   = loadClass_Unsafe(boxedFloatClassName)
  lazy val boxedDoubleClass: JClass  = loadClass_Unsafe(boxedDoubleClassName)
  lazy val boxedVoidClass: JClass    = loadClass_Unsafe(boxedVoidClassName)

  lazy val primitives: Map[String, JPrimitiveClass] = Map(
    "boolean" -> booleanClass, "byte" -> byteClass, "char" -> charClass, "short" -> shortClass, "int" -> intClass,
    "long" -> longClass, "float" -> floatClass, "double" -> doubleClass, "void" -> voidClass
  )

  lazy val typeClass: JClass = JTypeClass

  lazy val objectClass: JClass   = loadClass_Unsafe(objectClassName)
  lazy val stringClass: JClass   = loadClass_Unsafe(stringClassName)
  lazy val classClass: JClass    = loadClass_Unsafe(classClassName)
  lazy val iterableClass: JClass = loadClass_Unsafe(iterableClassName)
  lazy val errorClass: JClass    = loadClass_Unsafe(errorClassName)
  lazy val functionClass: JClass = loadClass_Unsafe(functionClassName)
  lazy val consumerClass: JClass = loadClass_Unsafe(consumerClassName)

  lazy val runtimeExceptionClass: JClass = loadClass_Unsafe(runtimeExceptionName)
  lazy val predefOperatorsClass: JClass  = loadClass_Unsafe(predefOperatorsName)


  def erase (signature: JTypeSignature, metaParams: List[FormalMetaParameter]): Try[JErasedType] = signature match {
    case prm: JPrimitiveTypeSignature    => Success(erase(prm))
    case arr: JArrayTypeSignature        => erase(arr, metaParams)
    case cls: JClassTypeSignature        => erase(cls)
    case cap: JCapturedWildcardSignature => cap.upperBound.map(erase(_, metaParams)).getOrElse(loadClass(objectClassName))
    case tvr: JTypeVariableSignature     => metaParams.find(_.name == tvr.name).flatMap(_.bounds.headOption).map(erase(_, metaParams)).getOrElse(loadClass(objectClassName))
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
    case SimpleClassTypeSignature(clazz, _)        => loadClass(clazz)
    case MemberClassTypeSignature(outer, clazz, _) => erase(outer).flatMap(loadInnerClass(_, clazz))
  }

  def erase_NoFail (signature: JClassTypeSignature): Option[JClass] = erase(signature) match {
    case Success(t) => Some(t)
    case Failure(e) =>
      error("class type not found : " + signature, e)
      None
  }

  def erase_NoFail(signature: JTypeSignature, metaParams: List[FormalMetaParameter]): Option[JErasedType] = erase(signature, metaParams) match {
    case Success(t) => Some(t)
    case Failure(e) =>
      error("type not found : " + signature, e)
      None
  }
}
