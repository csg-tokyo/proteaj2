package phenan.prj.internal

import phenan.prj._

class JLoadedClassType (clazz: JLoadedClass) extends JClassType {
  override def fields: Map[String, JField] = ???

  override def methods: Map[String, List[JMethod]] = ???
}

object JArrayClassType extends JClassType {
  override def fields: Map[String, JField] = Map.empty
  override def methods: Map[String, List[JMethod]] = Map.empty
}
