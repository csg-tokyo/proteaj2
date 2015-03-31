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

trait JMethod extends JMember {
  def name: String

  def erasedReturnType: JErasedType
  def erasedParameterTypes: List[JErasedType]

  def returnType: JGenericType
  def parameterTypes: List[JGenericType]
  def exceptionTypes: List[JGenericType]

  def overrides (that: JMethod): Boolean = {
    this.name == that.name && this.erasedReturnType.isSubclassOf(that.erasedReturnType) && this.erasedParameterTypes == that.erasedParameterTypes
  }
}

trait JConstructor extends JMember {

}