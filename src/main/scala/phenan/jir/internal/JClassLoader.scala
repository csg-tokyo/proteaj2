package phenan.jir.internal

import phenan.jir._
import phenan.jir.exception._

import scala.util._

import scalaz.Memo._

class JClassLoader (implicit config: JirConfig) {
  def load (name: String): Try[JClass] = {
    if (name.startsWith("[")) arrayDescriptor(name.tail)
    else loadClass(name)
  }

  def load (name: String, dim: Int): Try[JClass] = {
    if (dim > 0) loadClass(name).map(clazz => arrayOf(clazz, dim))
    else loadClass(name)
  }

  private[internal] def loadClass (name: String): Try[JLoadedClass] = mutableHashMapMemo(getClass)(name)

  def arrayOf (clazz: JClass): JClass = mutableHashMapMemo(getArray)(clazz)
  
  def arrayOf (clazz: JClass, dim: Int): JClass = {
    if (dim > 0) arrayOf(arrayOf(clazz), dim - 1)
    else clazz
  }

  def methodDescriptor (desc: String): Try[(List[JClass], JClass)] = {
    // "(" ParameterTypeDescriptors ")" ReturnTypeDescriptor
    if (desc.startsWith("(") && desc.contains(")")) {
      methodDescriptor(desc.take(desc.indexOf(')')).tail, desc.drop(desc.indexOf(')')).tail)
    }
    else Failure(InvalidClassFileException("invalid method descriptor : " + desc))
  }

  def fieldDescriptor (desc: String): Try[JClass] = typeDescriptor(desc).map(_._1)

  /* primitive types */

  def boolean: JClass = primitiveBoolean
  def byte   : JClass = primitiveByte
  def char   : JClass = primitiveChar
  def short  : JClass = primitiveShort
  def int    : JClass = primitiveInt
  def long   : JClass = primitiveLong
  def float  : JClass = primitiveFloat
  def double : JClass = primitiveDouble
  def void   : JClass = primitiveVoid

  private[internal] lazy val primitiveBoolean: JPrimitiveClass = new JPrimitiveClass("boolean", "java/lang/Boolean", this)
  private[internal] lazy val primitiveByte   : JPrimitiveClass = new JPrimitiveClass("byte", "java/lang/Byte", this)
  private[internal] lazy val primitiveChar   : JPrimitiveClass = new JPrimitiveClass("char", "java/lang/Character", this)
  private[internal] lazy val primitiveShort  : JPrimitiveClass = new JPrimitiveClass("short", "java/lang/Short", this)
  private[internal] lazy val primitiveInt    : JPrimitiveClass = new JPrimitiveClass("int", "java/lang/Integer", this)
  private[internal] lazy val primitiveLong   : JPrimitiveClass = new JPrimitiveClass("long", "java/lang/Long", this)
  private[internal] lazy val primitiveFloat  : JPrimitiveClass = new JPrimitiveClass("float", "java/lang/Float", this)
  private[internal] lazy val primitiveDouble : JPrimitiveClass = new JPrimitiveClass("double", "java/lang/Double", this)
  private[internal] lazy val primitiveVoid   : JPrimitiveClass = new JPrimitiveClass("void", "java/lang/Void", this)

  /* factory method for JClass ( without cache ) */

  private def getClass (name: String): Try[JLoadedClass] = fromClassFile(name)

  private def getArray (clazz: JClass): JClass = new JArrayClass(clazz, this)

  private def fromClassFile (name: String): Try[JLoadedClass] = findClassFile(name).map(cf => new JLoadedClass(cf, this))

  private def findClassFile (name: String): Try[BClassFile] = classPath.flatMap { cp =>
    cp.find(name).map(BClassFileParsers.fromStream) getOrElse {
      Failure(ClassFileNotFoundException("not found : " + name))
    }
  }

  private lazy val classPath = JClassPath.get

  /* helper methods for parsing descriptors */

  // desc does not include the first '['
  private def arrayDescriptor (desc: String): Try[JClass] = arrayDescriptor(desc, 1).map(_._1)

  private def methodDescriptor (params: String, returnType: String): Try[(List[JClass], JClass)] = for {
    ps  <- parameterDescriptors(params, Nil)
    ret <- returnTypeDescriptor(returnType)
  } yield (ps, ret)

  private def returnTypeDescriptor (desc: String): Try[JClass] = {
    if (desc.startsWith("V")) Success(primitiveVoid)
    else fieldDescriptor(desc)
  }

  // desc does not include '(' and ')'
  private def parameterDescriptors (desc: String, params: List[JClass]): Try[List[JClass]] = {
    if (desc.isEmpty) Success(params)
    else typeDescriptor(desc) match {
      case Success((cls, rest)) => parameterDescriptors(rest, params :+ cls)
      case Failure(e)           => Failure(e)
    }
  }

  private def arrayDescriptor (desc: String, dim: Int): Try[(JClass, String)] = {
    if (desc.isEmpty) Failure(InvalidClassFileException("invalid type descriptor : empty type descriptor"))
    else if (desc.head == '[') arrayDescriptor(desc.tail, dim + 1)
    else typeDescriptor(desc).map { case (component, rest) => (arrayOf(component, dim), rest) }
  }

  private def typeDescriptor (desc: String): Try[(JClass, String)] = {
    if (desc.isEmpty) Failure(InvalidClassFileException("invalid type descriptor : empty type descriptor"))
    else desc.head match {
      case 'B' => Success((primitiveByte, desc.tail))
      case 'C' => Success((primitiveChar, desc.tail))
      case 'D' => Success((primitiveDouble, desc.tail))
      case 'F' => Success((primitiveFloat, desc.tail))
      case 'I' => Success((primitiveInt, desc.tail))
      case 'J' => Success((primitiveLong, desc.tail))
      case 'L' => objectDescriptor(desc.tail)
      case 'S' => Success((primitiveShort, desc.tail))
      case 'Z' => Success((primitiveBoolean, desc.tail))
      case '[' => arrayDescriptor(desc.tail, 1)
      case h   => Failure(InvalidClassFileException("invalid method descriptor : " + h))
    }
  }

  // desc does not include the first 'L'
  private def objectDescriptor (desc: String): Try[(JClass, String)] = {
    val endOfDesc = desc.indexOf(';')
    loadClass(desc.take(endOfDesc)).map(cls => (cls, desc.drop(endOfDesc + 1)))
  }
}
