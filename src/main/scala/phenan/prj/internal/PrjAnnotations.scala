package phenan.prj.internal

import phenan.prj.{JTypeSignature, JMethodSignature, JClassSignature}

case class PrjClassAnnotations (signature: Option[JClassSignature], dsl: Option[PrjDSLAnnotation], isPure: Boolean, isContext: Boolean)

case class PrjMethodAnnotations (signature: Option[JMethodSignature], operator: Option[PrjOperatorAnnotation], isPure: Boolean, isFinalizer: Boolean)

case class PrjFieldAnnotations (isPure: Boolean)

case class PrjDSLAnnotation (priorities: List[String], withDSLs: List[String])

sealed trait PrjOperatorAnnotation {
  def association: PrjOperatorAssociation
  def pattern: List[PrjOperatorElement]
}

case class PrjStatementOperator (association: PrjOperatorAssociation, pattern: List[PrjOperatorElement]) extends PrjOperatorAnnotation
case class PrjExpressionOperator (association: PrjOperatorAssociation, pattern: List[PrjOperatorElement]) extends PrjOperatorAnnotation
case class PrjLiteralOperator (association: PrjOperatorAssociation, pattern: List[PrjOperatorElement]) extends PrjOperatorAnnotation

sealed trait PrjOperatorAssociation {
  def priority: String
}

case class PrjLeftAssociation (priority: String) extends PrjOperatorAssociation
case class PrjRightAssociation (priority: String) extends PrjOperatorAssociation
case class PrjNonAssociation (priority: String) extends PrjOperatorAssociation

sealed trait PrjOperatorElement

case class PrjOperatorName (name: String) extends PrjOperatorElement
case object PrjOperatorHole extends PrjOperatorElement
case object PrjOperatorRepStarHole extends PrjOperatorElement
case object PrjOperatorRepPlusHole extends PrjOperatorElement
case object PrjOperatorOptionalHole extends PrjOperatorElement
case class PrjOperatorAndPredicate (typeSig: JTypeSignature) extends PrjOperatorElement
case class PrjOperatorNotPredicate (typeSig: JTypeSignature) extends PrjOperatorElement
case class PrjOperatorPureValueRef (name: String) extends PrjOperatorElement
