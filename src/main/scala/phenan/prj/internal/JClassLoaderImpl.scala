package phenan.prj.internal

import phenan.prj._
import phenan.prj.state._
import phenan.prj.exception._

import scala.util._

import scalaz.Memo._

class JClassLoaderImpl (val compiler: JCompiler) extends JClassLoader {
  def load(name: String): Try[JErasedType] = {
    if (name.startsWith("[")) DescriptorParsers.parseArrayDescriptor(name) match {
      case Some(desc) => erase(desc, Nil)
      case None => Failure(InvalidTypeException("invalid type descriptor : " + name))
    }
    else loadClass(name)
  }

  def load(name: String, dim: Int): Try[JErasedType] = {
    if (dim > 0) loadClass(name).map(clazz => arrayOf(clazz, dim))
    else loadClass(name)
  }

  val loadClass: String => Try[JClass] = mutableHashMapMemo(getClass)

  val arrayOf: JErasedType => JArrayClass = mutableHashMapMemo(new JArrayClassImpl(_))

  def arrayOf(clazz: JErasedType, dim: Int): JErasedType = {
    if (dim > 0) arrayOf(arrayOf(clazz), dim - 1)
    else clazz
  }

  lazy val boolean: JPrimitiveClass = new JPrimitiveClassImpl("boolean", "java/lang/Boolean", compiler)
  lazy val byte: JPrimitiveClass = new JPrimitiveClassImpl("byte", "java/lang/Byte", compiler)
  lazy val char: JPrimitiveClass = new JPrimitiveClassImpl("char", "java/lang/Character", compiler)
  lazy val short: JPrimitiveClass = new JPrimitiveClassImpl("short", "java/lang/Short", compiler)
  lazy val int: JPrimitiveClass = new JPrimitiveClassImpl("int", "java/lang/Integer", compiler)
  lazy val long: JPrimitiveClass = new JPrimitiveClassImpl("long", "java/lang/Long", compiler)
  lazy val float: JPrimitiveClass = new JPrimitiveClassImpl("float", "java/lang/Float", compiler)
  lazy val double: JPrimitiveClass = new JPrimitiveClassImpl("double", "java/lang/Double", compiler)
  lazy val void: JPrimitiveClass = new JPrimitiveClassImpl("void", "java/lang/Void", compiler)

  /* factory method for JClass ( without cache ) */

  private def getClass(name: String): Try[JClass] = compiler.findIR(name).map(Success(_)).getOrElse {
    state.searchPath.find(name) match {
      case Some(cf: FoundClassFile) =>
        classFileParser.fromStream(cf.in).map(new JLoadedClass(_, compiler))
      case Some(sf: FoundSourceFile) =>
        compiler.generateIR(sf.in, sf.name)
        compiler.findIR(name).map(Success(_)).getOrElse(Failure(ClassFileNotFoundException("not found : " + name)))
      case None =>
        Failure(ClassFileNotFoundException("not found : " + name))
    }
  }

  private val classFileParser = new BClassFileParsers()
}
