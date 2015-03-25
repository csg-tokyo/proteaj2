package phenan.prj.internal

case class PrjClassAnnotations (signature: Option[PrjClassSignature], dsl: Option[PrjDSLAnnotation], isPure: Boolean, isContext: Boolean)

case class PrjMethodAnnotations (signature: Option[PrjMethodSignature], operator: Option[PrjOperatorAnnotation], isPure: Boolean, isFinalizer: Boolean)

case class PrjFieldAnnotations (isPure: Boolean)

case class PrjDSLAnnotation (priorities: List[String], withDSLs: List[String])

case class PrjClassSignature (genericParameters: List[PrjGenericParameter], superType: ClassTypeSignature, interfaces: List[ClassTypeSignature])

case class PrjMethodSignature (genericParameters: List[PrjGenericParameter], returnType: TypeSignature, parameterTypes: List[TypeSignature], throwsTypes: List[TypeSignature],
                               activates: List[TypeSignature], deactivates: List[TypeSignature], requires: List[TypeSignature])

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
case class PrjOperatorAndPredicate (typeSig: TypeSignature) extends PrjOperatorElement
case class PrjOperatorNotPredicate (typeSig: TypeSignature) extends PrjOperatorElement
case class PrjOperatorPureValueRef (name: String) extends PrjOperatorElement

case class PrjGenericParameter (name: String, parameterType: TypeSignature, bounds: List[TypeSignature])
