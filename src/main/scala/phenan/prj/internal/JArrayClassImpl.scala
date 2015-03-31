package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception._

import JModifier._

import scala.util._

class JArrayClassImpl (val component: JErasedType) extends JArrayClass {
  lazy val name = component.name + "[]"
  def compiler = component.compiler
}
