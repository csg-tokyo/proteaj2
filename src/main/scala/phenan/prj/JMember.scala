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

trait JProcedure extends JMember {
  def methodDef: JMethodDef
  def env: Map[String, MetaValue]

  def modifier: JModifier = methodDef.mod
  def returnType: JGenericType = JGenericType(methodDef.signature.returnType, env, compiler)
  def parameterTypes: List[JParameter] = methodDef.signature.parameters.map(sig => new JParameter(sig, env, compiler))
  def exceptionTypes: List[JGenericType] = methodDef.signature.throwTypes.map(sig => JGenericType(sig, env, compiler))

  def compiler = declaring.compiler
}

class JMethod (val methodDef: JMethodDef, val env: Map[String, MetaValue], val declaring: JModule, val clazz: JClass) extends JProcedure {
  def name: String = methodDef.name

  def erasedReturnType: JErasedType = methodDef.erasedReturnType
  def erasedParameterTypes: List[JErasedType] = methodDef.erasedParameterTypes

  def overrides (that: JMethod): Boolean = {
    this.name == that.name && this.erasedReturnType.isSubclassOf(that.erasedReturnType) && this.erasedParameterTypes == that.erasedParameterTypes
  }
}

class JConstructor (val methodDef: JMethodDef, val env: Map[String, MetaValue], val declaring: JObjectType) extends JProcedure

class JParameter (signature: JParameterSignature, env: Map[String, MetaValue], compiler: JCompiler) {
  lazy val contexts: List[JGenericType] = signature.contexts.map(sig => JGenericType(sig, env, compiler))
  lazy val genericType: JGenericType = JGenericType(signature.typeSig, env, compiler)
  def priority: Option[String] = signature.priority
  def varArgs: Boolean = signature.varArgs
  def defaultArg: Option[String] = signature.defaultArg
}
