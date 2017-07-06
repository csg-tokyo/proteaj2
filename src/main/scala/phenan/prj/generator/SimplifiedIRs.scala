package phenan.prj.generator

import phenan.prj._

/**
  * Created by ichikawa on 2017/07/05.
  */
trait SimplifiedIRs {
  this: JModules with JErasedTypes =>

  sealed trait SIRStatement

  case class SIRLocalDeclaration (localType: JType, name: String, init: Option[SIRExpression]) extends SIRStatement
  case class SIRReturnStatement (value: SIRSimpleExpression) extends SIRStatement


  sealed trait SIRExpression

  case class SIRContextSensitiveExpression (contexts: List[(JObjectType, String)], body: List[SIRStatement]) extends SIRExpression
  case class SIRArrayInit (componentType: JType, components: List[SIRSimpleExpression]) extends SIRExpression

  sealed trait SIRSimpleExpression extends SIRExpression

  case class SIRLocalRef (name: String) extends SIRSimpleExpression

  sealed trait SIRJavaLiteral extends SIRSimpleExpression

  case class SIRObjectClassLiteral (clazz: JClass, dim: Int) extends SIRJavaLiteral
  case class SIRPrimitiveClassLiteral (primitiveType: JPrimitiveType, dim: Int) extends SIRJavaLiteral
  case class SIRCharLiteral (value: Char) extends SIRJavaLiteral
  case class SIRIntLiteral (value: Int) extends SIRJavaLiteral
  case class SIRLongLiteral (value: Long) extends SIRJavaLiteral
  case class SIRBooleanLiteral (value: Boolean) extends SIRJavaLiteral
  case class SIRStringLiteral (value: String) extends SIRJavaLiteral
  case object SIRNullLiteral extends SIRJavaLiteral
}
