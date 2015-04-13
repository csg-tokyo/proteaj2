package phenan.prj

trait JMember {
  def modifier: JModifier
  def declaring: JModule

  def isPrivate: Boolean = modifier.check(JModifier.accPrivate)
}

class JField (fieldDef: JFieldDef, val fieldType: JType, val declaring: JModule) extends JMember {
  def modifier: JModifier = fieldDef.mod
  def name: String = fieldDef.name
}

class JMethod (val methodDef: JMethodDef, val env: Map[String, MetaValue], val declaring: JModule, val clazz: JClass) extends JMember {
  def modifier: JModifier = methodDef.mod
  def name: String = methodDef.name

  def erasedReturnType: JErasedType = methodDef.erasedReturnType
  def erasedParameterTypes: List[JErasedType] = methodDef.erasedParameterTypes

  def returnType: JGenericType = JGenericType(methodDef.signature.returnType, env, compiler)
  def parameterTypes: List[JGenericType] = methodDef.signature.paramTypes.map(sig => JGenericType(sig, env, compiler))
  def exceptionTypes: List[JGenericType] = methodDef.signature.throwTypes.map(sig => JGenericType(sig, env, compiler))

  def overrides (that: JMethod): Boolean = {
    this.name == that.name && this.erasedReturnType.isSubclassOf(that.erasedReturnType) && this.erasedParameterTypes == that.erasedParameterTypes
  }

  def compiler = declaring.compiler
}

class JConstructor (val methodDef: JMethodDef, val env: Map[String, MetaValue], val declaring: JObjectType) extends JMember {
  def modifier: JModifier = methodDef.mod

  def returnType: JGenericType = JGenericType(methodDef.signature.returnType, env, compiler)
  def parameterTypes: List[JGenericType] = methodDef.signature.paramTypes.map(sig => JGenericType(sig, env, compiler))
  def exceptionTypes: List[JGenericType] = methodDef.signature.throwTypes.map(sig => JGenericType(sig, env, compiler))

  def compiler = declaring.compiler
}