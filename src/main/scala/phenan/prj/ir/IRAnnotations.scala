package phenan.prj.ir

import phenan.prj._

case class IRClassAnnotations (signature: Option[JClassSignature], dsl: Option[DSLInfo], isPure: Boolean, isContext: Boolean, others: List[IROtherAnnotation])

case class IRMethodAnnotations (signature: Option[JMethodSignature], operator: Option[JSyntaxDef], isPure: Boolean, isFinalizer: Boolean, others: List[IROtherAnnotation])

case class IRFieldAnnotations (signature: Option[JTypeSignature], isPure: Boolean, others: List[IROtherAnnotation])

case class IROtherAnnotation (annotationType: JObjectType, args: Map[JMethod, MetaValue])