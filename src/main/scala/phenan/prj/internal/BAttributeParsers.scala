package phenan.prj.internal

class BAttributeParsers (classFile: BClassFile) {
  import classFile.poolReader._
  import BAttributeParsers._

  def parseClassAttributes (attributes: List[BAttributeInfo]): ClassAttributes = {
    var innerClassesAttribute: Option[InnerClassesAttribute] = None
    var signatureAttribute: Option[SignatureAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "InnerClasses" => innerClassesAttribute = Some(parse(attr.data)(innerClasses).get)
      case "Signature"    => signatureAttribute = Some(parse(attr.data)(signature).get)
      case _ =>
    }

    ClassAttributes(innerClassesAttribute, signatureAttribute)
  }

  def parseFieldAttribute (attributes: List[BAttributeInfo]): FieldAttributes = {
    var signatureAttribute: Option[SignatureAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "Signature"    => signatureAttribute = Some(parse(attr.data)(signature).get)
      case _ =>
    }

    FieldAttributes(signatureAttribute)
  }

  def parseMethodAttribute (attributes: List[BAttributeInfo]): MethodAttributes = {
    var signatureAttribute: Option[SignatureAttribute] = None
    var exceptionsAttribute: Option[ExceptionsAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "Signature"    => signatureAttribute = Some(parse(attr.data)(signature).get)
      case "Exceptions"   => exceptionsAttribute = Some(parse(attr.data)(exceptions).get)
      case _ =>
    }

    MethodAttributes(signatureAttribute, exceptionsAttribute)
  }
}

object BAttributeParsers extends ByteParsers {
  lazy val innerClasses: ByteParser[InnerClassesAttribute] = list(innerClassInfo) ^^ InnerClassesAttribute

  lazy val innerClassInfo: ByteParser[InnerClassInfo] = for {
    inner <- u2
    outer <- u2
    name  <- u2
    mod   <- u2
  } yield InnerClassInfo(inner, outer, name, mod)

  lazy val signature: ByteParser[SignatureAttribute] = u2 ^^ SignatureAttribute

  lazy val exceptions: ByteParser[ExceptionsAttribute] = list(u2) ^^ ExceptionsAttribute
}
