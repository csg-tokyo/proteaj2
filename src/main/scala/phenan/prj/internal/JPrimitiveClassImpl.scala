package phenan.prj.internal

import phenan.prj._

class JPrimitiveClassImpl (val name: String, val wrapperName: String, val loader: JClassLoader) extends JPrimitiveClass {
  lazy val wrapperClass = loader.loadClassOption(wrapperName)
  lazy val primitiveType: JPrimitiveType = ???
}
