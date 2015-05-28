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
  lazy val metaParameters = methodDef.signature.metaParams.map(param => param.name -> param).toMap
  lazy val returnType: JGenericType = JGenericType(methodDef.signature.returnType, env, compiler)
  lazy val parameterTypes: List[JParameter] = methodDef.signature.parameters.map(sig => new JParameter(sig, env, compiler))
  lazy val exceptionTypes: List[JGenericType] = methodDef.signature.throwTypes.map(sig => JGenericType(sig, env, compiler))

  def compiler = declaring.compiler
}

class JMethod (val methodDef: JMethodDef, val env: Map[String, MetaValue], val declaring: JModule, val clazz: JClass) extends JProcedure {
  def name: String = methodDef.name

  def erasedReturnType: JErasedType = methodDef.erasedReturnType
  def erasedParameterTypes: List[JErasedType] = methodDef.erasedParameterTypes

  lazy val syntax: Option[JSyntax] = methodDef.syntax.map { s =>
    JSyntax(s.priority.map(NamedPriority(_, clazz)).getOrElse(new UnnamedPriority(clazz.state.uniqueId, clazz)), translatePattern(s.syntax, Nil, parameterTypes))
  }

  def overrides (that: JMethod): Boolean = {
    this.name == that.name && this.erasedReturnType.isSubclassOf(that.erasedReturnType) && this.erasedParameterTypes == that.erasedParameterTypes
  }

  private def translatePattern (pattern: List[JSyntaxElementDef], result: List[JSyntaxElement], restParameters: List[JParameter]): List[JSyntaxElement] = pattern match {
    case (hole: JHoleDef) :: rest => restParameters match {
      case param :: ps => translatePattern(rest, result :+ translateHole(hole, param), ps)
      case Nil         => clazz.state.errorAndReturn("corresponding parameter cannot be found", result)
    }
    case JOperatorNameDef(name) :: rest => translatePattern(rest, result :+ JOperatorName(name), restParameters)
    case JMetaValueRefDef(name) :: rest if env.contains(name)            => translatePattern(rest, result :+ JMetaValue(env(name)), restParameters)
    case JMetaValueRefDef(name) :: rest if metaParameters.contains(name) => compiler.typeLoader.fromTypeSignature(metaParameters(name).metaType, env) match {
      case Some(metaType) => translatePattern(rest, result :+ JMetaOperand(metaType), restParameters)
      case None           => clazz.state.errorAndReturn("invalid meta type of meta parameter " + name, result)
    }
    case JMetaValueRefDef(name) :: rest => clazz.state.errorAndReturn("meta parameter " + name + " cannot be found", result)
    case (pred: JPredicateDef) :: rest  => translatePattern(rest, result :+ translatePredicate(pred), restParameters)
    case Nil if restParameters.isEmpty  => result
    case Nil                            => clazz.state.errorAndReturn("corresponding operand cannot be found", result)
  }

  private def translateHole (elem: JHoleDef, param: JParameter): JSyntaxElement = elem match {
    case JOperandDef         => JOperand(param)
    case JOptionalOperandDef => JOptionalOperand(param)
    case JRepetition0Def     => JRepetition0(param)
    case JRepetition1Def     => JRepetition1(param)
  }

  private def translatePredicate (elem: JPredicateDef): JSyntaxElement = elem match {
    case JAndPredicateDef(sig) => JAndPredicate(new JParameter(sig, env, compiler))
    case JNotPredicateDef(sig) => JNotPredicate(new JParameter(sig, env, compiler))
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
