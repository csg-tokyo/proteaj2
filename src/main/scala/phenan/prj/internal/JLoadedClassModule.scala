package phenan.prj.internal

import phenan.prj._

class JLoadedClassModule (clazz: JLoadedClass) extends JClassModule {
  override def fields: Map[String, JField] = ???

  override def methods: Map[String, List[JMethod]] = ???

  def compiler = clazz.compiler
}
