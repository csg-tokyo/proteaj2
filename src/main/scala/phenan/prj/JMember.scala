package phenan.prj

trait JMember {
  def modifier: JModifier
  def declaring: JModule

  def isPrivate: Boolean = modifier.check(JModifier.accPrivate)
}

trait JField extends JMember {
  def name: String
  def fieldType: JType
}

trait JGenMethod extends JMember {
  def name: String

  def returnType: JType
  def parameterTypes: List[JType]
  def exceptionTypes: List[JObjectType]

  def overrides (that: JGenMethod): Boolean = {
    this.name == that.name && this.returnType <:< that.returnType && this.parameterTypes == that.parameterTypes
  }

  def bind (typeArgs: List[MetaValue]): Option[JMethod]
  def infer (returnType: JType): Option[JMethod]
  def infer (argTypes: List[JType]): Option[JMethod]
}

trait JMethod {

}

trait JGenConstructor extends JMember {

}