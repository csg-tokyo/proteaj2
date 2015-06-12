package phenan.prj.ir

import phenan.prj._

case class IRClassAnnotations (signature: Option[JClassSignature], dsl: Option[DSLInfo], isPure: Boolean, isContext: Boolean, others: List[IRAnnotation])

case class IRMethodAnnotations (signature: Option[JMethodSignature], operator: Option[JOperatorSyntaxDef], isPure: Boolean, isFinalizer: Boolean, others: List[IRAnnotation])

case class IRFieldAnnotations (signature: Option[JTypeSignature], isPure: Boolean, others: List[IRAnnotation])

case class IRAnnotation (annotationType: JObjectType, args: Map[JMethod, MetaValue])