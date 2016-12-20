package phenan.prj.internal

import phenan.prj._

class JArrayClassImpl (val component: JErasedType) extends JArrayClass {
  lazy val name: String = component.name + "[]"
  def compiler: JCompiler = component.compiler
}
