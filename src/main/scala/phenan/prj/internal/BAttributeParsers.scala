package phenan.prj.internal

import phenan.prj.state.JState

import scala.util._

class BAttributeParsers (classFile: BClassFile)(implicit state: JState) {
  import classFile.poolReader._
  import BAttributeParsers._

  def parseClassAttributes (attributes: List[BAttributeInfo]): ClassAttributes = {
    var innerClassesAttribute: Option[InnerClassesAttribute] = None
    var signatureAttribute: Option[SignatureAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "InnerClasses" => innerClassesAttribute = parseAttr(attr)(innerClasses)
      case "Signature"    => signatureAttribute = parseAttr(attr)(signature)
      case _ =>
    }

    ClassAttributes(innerClassesAttribute, signatureAttribute)
  }

  def parseFieldAttribute (attributes: List[BAttributeInfo]): FieldAttributes = {
    var signatureAttribute: Option[SignatureAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "Signature"    => signatureAttribute = parseAttr(attr)(signature)
      case _ =>
    }

    FieldAttributes(signatureAttribute)
  }

  def parseMethodAttribute (attributes: List[BAttributeInfo]): MethodAttributes = {
    var signatureAttribute: Option[SignatureAttribute] = None
    var exceptionsAttribute: Option[ExceptionsAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "Signature"    => signatureAttribute = parseAttr(attr)(signature)
      case "Exceptions"   => exceptionsAttribute = parseAttr(attr)(exceptions)
      case _ =>
    }

    MethodAttributes(signatureAttribute, exceptionsAttribute)
  }

  private def parseAttr [T] (attr: BAttributeInfo)(parser: ByteParser[T]): Option[T] = {
    parse(attr.data)(parser) match {
      case Success(result) => Some(result)
      case Failure(e)      =>
        state.error("broken class file : cannot parse attribute", e)
        None
    }
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
