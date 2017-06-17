package phenan.prj.internal

import phenan.prj._

case class JClassAnnotations (signature: Option[JClassSignature] = None, dsl: Option[DSLInfo] = None, isPure: Boolean = false)

case class JMethodAnnotations (signature: Option[JMethodSignature] = None, operator: Option[JSyntaxDef] = None, isPure: Boolean = false, isFinalizer: Boolean = false)

case class JFieldAnnotations (signature: Option[JTypeSignature] = None, isPure: Boolean = false)
