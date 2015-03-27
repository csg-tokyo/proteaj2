package phenan.prj

trait JGenMethod extends JMember {
  def name: String

  def returnType: JType
  def parameterTypes: List[JType]
  def exceptionTypes: List[JObjectType]

  def overrides (that: JGenMethod): Boolean = {
    this.name == that.name && this.returnType <:< that.returnType && this.parameterTypes == that.parameterTypes
  }

  def bind (typeArgs: List[JType]): Option[JMethod]
  def infer (returnType: JType): Option[JMethod]
  def infer (argTypes: List[JType]): Option[JMethod]
}

trait JMethod {

}
