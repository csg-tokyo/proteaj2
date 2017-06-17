package phenan.prj.internal

sealed trait BAttribute

case class InnerClassesAttribute (classes: List[InnerClassInfo]) extends BAttribute

case class InnerClassInfo (innerClassInfo: Int, outerClassInfo: Int, innerName: Int, mod: Int)

case class SignatureAttribute (signature: Int) extends BAttribute

case class ExceptionsAttribute (exceptions: List[Int]) extends BAttribute

case class RuntimeVisibleAnnotationsAttribute (annotations: List[BAnnotation]) extends BAttribute


case class BAnnotation (annotationType: Int, values: List[(Int, BAnnotationElement)])

sealed trait BAnnotationElement

case class BAnnotationElement_Byte (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Char (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Double (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Float (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Int (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Long (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Short (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Boolean (ref: Int) extends BAnnotationElement
case class BAnnotationElement_String (ref: Int) extends BAnnotationElement
case class BAnnotationElement_Enum (enumType: Int, constName: Int) extends BAnnotationElement
case class BAnnotationElement_Class (classInfo: Int) extends BAnnotationElement
case class BAnnotationElement_Annotation (annotation: BAnnotation) extends BAnnotationElement
case class BAnnotationElement_Array (array: List[BAnnotationElement]) extends BAnnotationElement


case class ClassAttributes
(innerClasses: Option[InnerClassesAttribute] = None,
 signature: Option[SignatureAttribute] = None,
 annotations: Option[RuntimeVisibleAnnotationsAttribute] = None)

case class FieldAttributes
(signature: Option[SignatureAttribute] = None,
 annotations: Option[RuntimeVisibleAnnotationsAttribute] = None)

case class MethodAttributes
(signature: Option[SignatureAttribute] = None,
 exceptions: Option[ExceptionsAttribute] = None,
 annotations: Option[RuntimeVisibleAnnotationsAttribute] = None)