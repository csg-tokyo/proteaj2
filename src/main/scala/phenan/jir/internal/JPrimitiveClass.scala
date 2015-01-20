package phenan.jir.internal

import phenan.jir._
import JModifier._

class JPrimitiveClass (val name: String, val wrapperName: String, val loader: JClassLoader) extends JClass {
  override def mod = JModifier(accPublic | accFinal)

  override def superClass: Option[JClass] = None
  override def interfaces: List[JClass] = Nil

  override def innerClasses: Map[String, JClass] = Map.empty
  override def outerClass: Option[JClass] = None

  override def fields: List[JFieldDef] = Nil
  override def methods: List[JMethodDef] = Nil

  lazy val wrapperClass = loader.loadClass(wrapperName)
}
