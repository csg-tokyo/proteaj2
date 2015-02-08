package phenan.prj.internal

import phenan.prj._
import phenan.prj.state._
import phenan.prj.exception._

import scala.util._

import scalaz.Memo._

class JClassLoader (jdc: JCompiler)(implicit state: JState) {
  def load(name: String): Try[JErasedType] = {
    if (name.startsWith("[")) arrayDescriptor(name.tail)
    else loadClass(name)
  }

  def load(name: String, dim: Int): Try[JErasedType] = {
    if (dim > 0) loadClass(name).map(clazz => arrayOf(clazz, dim))
    else loadClass(name)
  }

  val arrayOf: JErasedType => JArrayClass = mutableHashMapMemo(getArray)

  def arrayOf(clazz: JErasedType, dim: Int): JErasedType = {
    if (dim > 0) arrayOf(arrayOf(clazz), dim - 1)
    else clazz
  }

  lazy val boolean: JPrimitiveClass = new JPrimitiveClassImpl("boolean", "java/lang/Boolean", this)
  lazy val byte: JPrimitiveClass = new JPrimitiveClassImpl("byte", "java/lang/Byte", this)
  lazy val char: JPrimitiveClass = new JPrimitiveClassImpl("char", "java/lang/Character", this)
  lazy val short: JPrimitiveClass = new JPrimitiveClassImpl("short", "java/lang/Short", this)
  lazy val int: JPrimitiveClass = new JPrimitiveClassImpl("int", "java/lang/Integer", this)
  lazy val long: JPrimitiveClass = new JPrimitiveClassImpl("long", "java/lang/Long", this)
  lazy val float: JPrimitiveClass = new JPrimitiveClassImpl("float", "java/lang/Float", this)
  lazy val double: JPrimitiveClass = new JPrimitiveClassImpl("double", "java/lang/Double", this)
  lazy val void: JPrimitiveClass = new JPrimitiveClassImpl("void", "java/lang/Void", this)

  /* package private methods */
  private[internal] def loadClassOption (name: String): Option[JClass] = {
    loadClass(name) match {
      case Success(clazz) => Some(clazz)
      case Failure(e)     =>
        state.error("fail to load class " + name, e)
        None
    }
  }

  private[internal] val loadClass: String => Try[JClass] = mutableHashMapMemo(getClass)

  private[internal] def methodDescriptor(desc: String): Option[(List[JErasedType], JErasedType)] = {
    // "(" ParameterTypeDescriptors ")" ReturnTypeDescriptor
    if (desc.startsWith("(") && desc.contains(")")) {
      methodDescriptor(desc.take(desc.indexOf(')')).tail, desc.drop(desc.indexOf(')')).tail) match {
        case Success(result) => Some(result)
        case Failure(e)      =>
          state.error("invalid method descriptor : " + desc, e)
          None
      }
    }
    else {
      state.error("invalid method descriptor : " + desc)
      None
    }
  }

  private[internal] def fieldDescriptor(desc: String): Option[JErasedType] = typeDescriptor(desc).map(_._1) match {
    case Success(result) => Some(result)
    case Failure(e)      =>
      state.error("invalid field descriptor : " + desc, e)
      None
  }

  /* factory method for JClass ( without cache ) */

  private def getClass(name: String): Try[JClass] = {
    jdc.findCompiling(name).map(Success(_)) getOrElse state.searchPath.find(name) match {
      case Some(cf: FoundClassFile)  => classFileParser.fromStream(cf.in).map(new JLoadedClass(_, this))
      case Some(sf: FoundSourceFile) => jdc.compile(sf.in)
      case None => Failure(ClassFileNotFoundException("not found : " + name))
    }
  }

  private val classFileParser = new BClassFileParsers()

  private def getArray (clazz: JErasedType): JArrayClass = new JArrayClassImpl(clazz, this)

  /* helper methods for parsing descriptors */

  // desc does not include the first '['
  private def arrayDescriptor (desc: String): Try[JErasedType] = arrayDescriptor(desc, 1).map(_._1)

  private def methodDescriptor (params: String, returnType: String): Try[(List[JErasedType], JErasedType)] = for {
    ps  <- parameterDescriptors(params, Nil)
    ret <- returnTypeDescriptor(returnType)
  } yield (ps, ret)

  private def returnTypeDescriptor (desc: String): Try[JErasedType] = {
    if (desc.startsWith("V")) Success(void)
    else typeDescriptor(desc).map(_._1)
  }

  // desc does not include '(' and ')'
  private def parameterDescriptors (desc: String, params: List[JErasedType]): Try[List[JErasedType]] = {
    if (desc.isEmpty) Success(params)
    else typeDescriptor(desc) match {
      case Success((cls, rest)) => parameterDescriptors(rest, params :+ cls)
      case Failure(e)           => Failure(e)
    }
  }

  private def arrayDescriptor (desc: String, dim: Int): Try[(JErasedType, String)] = {
    if (desc.isEmpty) Failure(InvalidClassFileException("invalid type descriptor : empty type descriptor"))
    else if (desc.head == '[') arrayDescriptor(desc.tail, dim + 1)
    else typeDescriptor(desc).map { case (component, rest) => (arrayOf(component, dim), rest) }
  }

  private def typeDescriptor (desc: String): Try[(JErasedType, String)] = {
    if (desc.isEmpty) Failure(InvalidClassFileException("invalid type descriptor : empty type descriptor"))
    else desc.head match {
      case 'B' => Success((byte, desc.tail))
      case 'C' => Success((char, desc.tail))
      case 'D' => Success((double, desc.tail))
      case 'F' => Success((float, desc.tail))
      case 'I' => Success((int, desc.tail))
      case 'J' => Success((long, desc.tail))
      case 'L' => objectDescriptor(desc.tail)
      case 'S' => Success((short, desc.tail))
      case 'Z' => Success((boolean, desc.tail))
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
