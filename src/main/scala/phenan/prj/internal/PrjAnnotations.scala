package phenan.prj.internal

import phenan.prj._

case class PrjClassAnnotations (signature: Option[JClassSignature], dsl: Option[PrjDSLAnnotation], isPure: Boolean, isContext: Boolean)

case class PrjMethodAnnotations (signature: Option[JMethodSignature], operator: Option[JOperatorSyntax], isPure: Boolean, isFinalizer: Boolean)

case class PrjFieldAnnotations (signature: Option[JTypeSignature], isPure: Boolean)

case class PrjDSLAnnotation (priorities: List[String], withDSLs: List[String])
