package phenan.prj.internal

import phenan.prj._

case class PrjClassAnnotations (signature: Option[JClassSignature], dsl: Option[DSLInfo], isPure: Boolean, isContext: Boolean)

case class PrjMethodAnnotations (signature: Option[JMethodSignature], operator: Option[JSyntaxDef], isPure: Boolean, isFinalizer: Boolean)

case class PrjFieldAnnotations (signature: Option[JTypeSignature], isPure: Boolean)
