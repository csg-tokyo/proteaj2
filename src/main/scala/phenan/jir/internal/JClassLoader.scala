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

  def loadClass (name: String): Try[JClass] = mutableHashMapMemo(getClass)(name)

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

  lazy val primitiveBoolean: JClass = new JPrimitiveClass("boolean", "java/lang/Boolean", this)
  lazy val primitiveByte   : JClass = new JPrimitiveClass("byte", "java/lang/Byte", this)
  lazy val primitiveChar   : JClass = new JPrimitiveClass("char", "java/lang/Character", this)
  lazy val primitiveShort  : JClass = new JPrimitiveClass("short", "java/lang/Short", this)
  lazy val primitiveInt    : JClass = new JPrimitiveClass("int", "java/lang/Integer", this)
  lazy val primitiveLong   : JClass = new JPrimitiveClass("long", "java/lang/Long", this)
  lazy val primitiveFloat  : JClass = new JPrimitiveClass("float", "java/lang/Float", this)
  lazy val primitiveDouble : JClass = new JPrimitiveClass("double", "java/lang/Double", this)
  lazy val primitiveVoid   : JClass = new JPrimitiveClass("void", "java/lang/Void", this)

  /* factory method for JClass ( without cache ) */

  private def getClass (name: String): Try[JClass] = findClassFile(name).map(cf => new JLoadedClass(cf, this))

  private def getArray (clazz: JClass): JClass = new JArrayClass(clazz, this)

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
