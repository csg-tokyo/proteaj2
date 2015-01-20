package phenan.jir.internal

import phenan.jir._
import JModifier._

class JArrayClass (val component: JClass, val loader: JClassLoader) extends JClass {
  lazy val name = component.name + "[]"

  override def mod = JModifier(accPublic | accFinal | accSuper)

  override def superClass: Option[JClass] = Some(loader.loadClass("java/lang/Object").get)
  override def interfaces: List[JClass] = List(loader.loadClass("java/io/Serializable").get, loader.loadClass("java/lang/Cloneable").get)

  override def innerClasses: Map[String, JClass] = Map.empty
  override def outerClass: Option[JClass] = None

  lazy val fields: List[JFieldDef] = List(new JArrayFieldDef(this))
  lazy val methods: List[JMethodDef] = List(new JArrayMethodDef(this))
}

class JArrayFieldDef (val declaringClass: JArrayClass) extends JFieldDef {
  override def mod = JModifier(accPublic | accFinal)
  override def name = "length"
  override def fieldClass = declaringClass.loader.loadClass("int").get
}

class JArrayMethodDef (val declaringClass: JArrayClass) extends JMethodDef {
  override def mod = JModifier(accPublic | accFinal)
  override def name = "clone"
  override def paramClasses: List[JClass] = Nil
  override def returnClass: JClass = declaringClass
  override def exceptions: List[JClass] = Nil
}
