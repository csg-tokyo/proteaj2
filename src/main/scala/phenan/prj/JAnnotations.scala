package phenan.prj

case class JClassAnnotations (signature: Option[JClassSignature], dsl: Option[DSLInfo], isPure: Boolean, isContext: Boolean, others: List[UnknownAnnotation])

case class JMethodAnnotations (signature: Option[JMethodSignature], operator: Option[JSyntaxDef], isPure: Boolean, isFinalizer: Boolean, others: List[UnknownAnnotation])

case class JFieldAnnotations (signature: Option[JTypeSignature], isPure: Boolean, others: List[UnknownAnnotation])

trait UnknownAnnotation
