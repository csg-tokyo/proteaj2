package phenan.prj.ir

import phenan.prj._

class IRClass extends JClass {
  override def mod: JModifier = ???

  override def interfaces: List[JClass] = ???

  override def methods: List[IRProcedureDef] = ???

  override def outerClass: Option[JClass] = ???

  override def objectType(typeArgs: List[JValueType]): Option[JObjectType] = ???

  override def fields: List[JFieldDef] = ???

  override def superClass: Option[JClass] = ???

  override def classType: JClassType = ???

  override def innerClasses: Map[String, JClass] = ???

  override def name: String = ???
}
