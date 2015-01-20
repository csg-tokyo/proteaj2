package phenan.jir.internal

sealed trait BAttribute

case class InnerClassesAttribute (classes: List[InnerClassInfo]) extends BAttribute

case class InnerClassInfo (innerClassInfo: Int, outerClassInfo: Int, innerName: Int, mod: Int)

case class SignatureAttribute (signature: Int) extends BAttribute

case class ExceptionsAttribute (exceptions: List[Int]) extends BAttribute




case class ClassAttributes (innerClasses: Option[InnerClassesAttribute], signature: Option[SignatureAttribute])

case class FieldAttributes (signature: Option[SignatureAttribute])

case class MethodAttributes (signature: Option[SignatureAttribute], exceptions: Option[ExceptionsAttribute])