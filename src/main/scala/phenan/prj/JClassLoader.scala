package phenan.prj

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

  def arrayOf: JErasedType => JArrayClass
  def arrayOf(clazz: JErasedType, dim: Int): JErasedType

  def methodDescriptor(desc: String): Option[(List[JErasedType], JErasedType)]
  def fieldDescriptor(desc: String): Option[JErasedType]

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

  lazy val objectClass: JClass = loadClass("java/lang/Object").get

  implicit def state: JState
}
