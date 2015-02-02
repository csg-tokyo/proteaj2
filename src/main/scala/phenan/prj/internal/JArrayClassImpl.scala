package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception._

import JModifier._

import scala.util._

class JArrayClassImpl (val component: JErasedType, val loader: JClassLoader) extends JArrayClass {
  lazy val name = component.name + "[]"

//  lazy val fields: List[JFieldDef] = List(new JArrayFieldDef(this))
//  lazy val methods: List[JMethodDef] = List(new JArrayMethodDef(this))
}
/*
class JArrayFieldDef (val declaringClass: JArrayClass) extends JFieldDef {
  override def mod = JModifier(accPublic | accFinal)
  override def name = "length"
  override def fieldClass = declaringClass.loader.load("int").get
}

class JArrayMethodDef (val declaringClass: JArrayClass) extends JMethodDef {
  override def mod = JModifier(accPublic | accFinal)
  override def name = "clone"
  override def paramClasses: List[JClass] = Nil
  override def returnClass: JClass = declaringClass
  override def exceptions: List[JClass] = Nil
}
*/