package phenan.prj.internal

import phenan.prj._
import JModifier._
import phenan.prj.exception.InvalidTypeException

import scala.util.{Failure, Success, Try}

class JPrimitiveClassImpl (val name: String, val wrapperName: String, val loader: JClassLoader) extends JPrimitiveClass {
  lazy val wrapperClass = loader.loadClass(wrapperName).get
  lazy val primitiveType: JPrimitiveType = ???
}
