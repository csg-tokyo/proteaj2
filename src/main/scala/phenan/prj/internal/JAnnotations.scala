package phenan.prj.internal

import phenan.prj._

case class JClassAnnotations (signature: Option[JClassSignature], dsl: Option[DSLInfo], isPure: Boolean)

case class JMethodAnnotations (signature: Option[JMethodSignature], operator: Option[JSyntaxDef], isPure: Boolean, isFinalizer: Boolean)

case class JFieldAnnotations (signature: Option[JTypeSignature], isPure: Boolean)
