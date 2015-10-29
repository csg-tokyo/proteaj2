package phenan.prj

trait JMember {
  def modifier: JModifier
  def declaring: JModule

  def isPrivate: Boolean = modifier.check(JModifier.accPrivate)
  def isProtected: Boolean = modifier.check(JModifier.accProtected)
  def isPublic: Boolean = modifier.check(JModifier.accPublic)
}

case class JField (fieldDef: JFieldDef, fieldType: JType, declaring: JModule) extends JMember {
  def modifier: JModifier = fieldDef.mod
  def name: String = fieldDef.name
  def declaringClass = fieldDef.declaringClass
}

trait JProcedure extends JMember {
  def methodDef: JMethodDef
  def env: Map[String, MetaArgument]

  def modifier: JModifier = methodDef.mod
  def declaringClass = methodDef.declaringClass

  lazy val metaParameters = methodDef.signature.metaParams.map(param => param.name -> param).toMap
  lazy val returnType: JGenericType = JGenericType(methodDef.signature.returnType, env, compiler)
  lazy val returnBounds: List[JGenericType] = methodDef.signature.returnBounds.map(sig => JGenericType(sig, env, compiler))
  lazy val parameterTypes: List[JParameter] = methodDef.signature.parameters.map(sig => JParameter(sig, env, compiler))
  lazy val exceptionTypes: List[JGenericType] = methodDef.signature.throwTypes.map(sig => JGenericType(sig, env, compiler))

  lazy val activates: List[JGenericType] = methodDef.signature.activates.map(sig => JGenericType(sig, env, compiler))
  lazy val deactivates: List[JGenericType] = methodDef.signature.deactivates.map(sig => JGenericType(sig, env, compiler))
  lazy val requires: List[JGenericType] = methodDef.signature.requires.map(sig => JGenericType(sig, env, compiler))

  lazy val syntax: Option[JSyntax] = methodDef.syntax.map {
    case JExpressionSyntaxDef(p, s) => JExpressionSyntax(p, translatePattern(s, Nil, parameterTypes))
    case JLiteralSyntaxDef(p, s)    => JLiteralSyntax(p, translatePattern(s, Nil, parameterTypes))
    case JStatementSyntaxDef(p, s)  => ???
  }

  private def translatePattern (pattern: List[JSyntaxElementDef], result: List[JSyntaxElement], restParameters: List[JParameter]): List[JSyntaxElement] = pattern match {
    case (hole: JHoleDef) :: rest => restParameters match {
      case param :: ps => translatePattern(rest, result :+ translateHole(hole, param), ps)
      case Nil         => compiler.state.errorAndReturn("corresponding parameter cannot be found", result)
    }
    case JOperatorNameDef(name) :: rest => translatePattern(rest, result :+ JOperatorName(name), restParameters)
    case (mv: JMetaValueRefDef) :: rest => translateMetaValueRef(mv) match {
      case Some(e) => translatePattern(rest, result :+ e, restParameters)
      case None    => compiler.state.errorAndReturn("meta parameter " + mv.name + " cannot be found", result)
    }
    case (pred: JPredicateDef) :: rest  => translatePattern(rest, result :+ translatePredicate(pred), restParameters)
    case Nil if restParameters.isEmpty  => result
    case Nil                            => compiler.state.errorAndReturn("corresponding operand cannot be found", result)
  }

  private def translateHole (elem: JHoleDef, param: JParameter): JSyntaxElement = elem match {
    case JOperandDef(p)         => JOperand(param, p)
    case JOptionalOperandDef(p) => JOptionalOperand(param, p)
    case JRepetition0Def(p)     => JRepetition0(param, p)
    case JRepetition1Def(p)     => JRepetition1(param, p)
    case JRegexNameDef(name)    => JRegexName(name)
  }

  private def translateMetaValueRef (mv: JMetaValueRefDef): Option[JSyntaxElement] = {
    if (env.contains(mv.name)) Some(JMetaName(mv.name, env(mv.name), mv.priority))
    else if (metaParameters.contains(mv.name)) {
      val mp = metaParameters(mv.name)
      Some(JMetaOperand(mv.name, JParameter(mp.metaType, env, compiler), mv.priority))
    }
    else None
  }

  private def translatePredicate (elem: JPredicateDef): JSyntaxElement = elem match {
    case JAndPredicateDef(sig, p) => JAndPredicate(JParameter(sig, env, compiler), p)
    case JNotPredicateDef(sig, p) => JNotPredicate(JParameter(sig, env, compiler), p)
  }

  def compiler = declaring.compiler
}

class JMethod (val methodDef: JMethodDef, val env: Map[String, MetaArgument], val declaring: JModule) extends JProcedure {
  def name: String = methodDef.name

  def erasedReturnType: JErasedType = methodDef.erasedReturnType
  def erasedParameterTypes: List[JErasedType] = methodDef.erasedParameterTypes

  def overrides (that: JMethod): Boolean = {
    this.name == that.name && this.erasedReturnType.isSubclassOf(that.erasedReturnType) && this.erasedParameterTypes == that.erasedParameterTypes
  }
}

class JConstructor (val methodDef: JMethodDef, val env: Map[String, MetaArgument], val declaring: JObjectType) extends JProcedure

case class JParameter (signature: JParameterSignature, env: Map[String, MetaArgument], compiler: JCompiler) {
  lazy val contexts: List[JGenericType] = signature.contexts.map(sig => JGenericType(sig, env, compiler))
  lazy val genericType: JGenericType = JGenericType(signature.typeSig, env, compiler)
  def varArgs: Boolean = signature.varArgs
  def defaultArg: Option[String] = signature.defaultArg
}

object JParameter {
  def apply (sig: JTypeSignature, env: Map[String, MetaArgument], compiler: JCompiler): JParameter = {
    JParameter(JParameterSignature(Nil, sig, false, None), env, compiler)
  }
}