package phenan.prj

trait JGenMethod extends JMember {
  def name: String

  def returnType: JValueType
  def parameterTypes: List[JValueType]
  def exceptionTypes: List[JObjectType]

  def overrides (that: JGenMethod): Boolean = {
    this.name == that.name && this.returnType <:< that.returnType && this.parameterTypes == that.parameterTypes
  }

  def bind (typeArgs: List[JValueType]): Option[JMethod]
  def infer (returnType: JValueType): Option[JMethod]
  def infer (argTypes: List[JValueType]): Option[JMethod]
}

trait JMethod {

}
