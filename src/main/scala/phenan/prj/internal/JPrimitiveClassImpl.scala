package phenan.prj.internal

import phenan.prj._

case class JPrimitiveClassImpl (name: String, wrapperName: String, compiler: JCompiler) extends JPrimitiveClass {
  lazy val wrapperClass = compiler.classLoader.loadClass_PE(wrapperName)
  lazy val primitiveType: JPrimitiveType = ???
}
