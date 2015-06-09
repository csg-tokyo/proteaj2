package phenan.prj.internal

import phenan.prj.combinator._
import phenan.prj.exception.InvalidClassFileException
import phenan.prj.state.JState

import scala.util._

class BAttributeParsers (classFile: BClassFile)(implicit state: JState) {
  import classFile.poolReader._
  import BAttributeParsers._

  def parseClassAttributes (attributes: List[BAttributeInfo]): ClassAttributes = {
    var innerClassesAttribute: Option[InnerClassesAttribute] = None
    var signatureAttribute: Option[SignatureAttribute] = None
    var annotationsAttribute: Option[RuntimeVisibleAnnotationsAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "InnerClasses" => innerClassesAttribute = parseAttr(attr)(innerClasses)
      case "Signature"    => signatureAttribute = parseAttr(attr)(signature)
      case "RuntimeVisibleAnnotations" => annotationsAttribute = parseAttr(attr)(runtimeVisibleAnnotations)
      case _ =>
    }

    ClassAttributes(innerClassesAttribute, signatureAttribute, annotationsAttribute)
  }

  def parseFieldAttribute (attributes: List[BAttributeInfo]): FieldAttributes = {
    var signatureAttribute: Option[SignatureAttribute] = None
    var annotationsAttribute: Option[RuntimeVisibleAnnotationsAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "Signature"    => signatureAttribute = parseAttr(attr)(signature)
      case "RuntimeVisibleAnnotations" => annotationsAttribute = parseAttr(attr)(runtimeVisibleAnnotations)
      case _ =>
    }

    FieldAttributes(signatureAttribute, annotationsAttribute)
  }

  def parseMethodAttribute (attributes: List[BAttributeInfo]): MethodAttributes = {
    var signatureAttribute: Option[SignatureAttribute] = None
    var exceptionsAttribute: Option[ExceptionsAttribute] = None
    var annotationsAttribute: Option[RuntimeVisibleAnnotationsAttribute] = None

    for (attr <- attributes) readAs[BUtf8Value](attr.tag).value match {
      case "Signature"    => signatureAttribute = parseAttr(attr)(signature)
      case "Exceptions"   => exceptionsAttribute = parseAttr(attr)(exceptions)
      case "RuntimeVisibleAnnotations" => annotationsAttribute = parseAttr(attr)(runtimeVisibleAnnotations)
      case _ =>
    }

    MethodAttributes(signatureAttribute, exceptionsAttribute, annotationsAttribute)
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

  lazy val runtimeVisibleAnnotations: ByteParser[RuntimeVisibleAnnotationsAttribute] = list(annotation) ^^ RuntimeVisibleAnnotationsAttribute

  lazy val annotation: ByteParser[BAnnotation] = ref(u2 >>= { annType =>
    list(for(elemName <- u2; value <- annotationElement) yield elemName -> value) ^^ { BAnnotation(annType, _) }
  })

  lazy val annotationElement: ByteParser[BAnnotationElement] = ref(u1 >>= {
    case 'B' => u2 ^^ BAnnotationElement_Byte
    case 'C' => u2 ^^ BAnnotationElement_Char
    case 'D' => u2 ^^ BAnnotationElement_Double
    case 'F' => u2 ^^ BAnnotationElement_Float
    case 'I' => u2 ^^ BAnnotationElement_Int
    case 'J' => u2 ^^ BAnnotationElement_Long
    case 'S' => u2 ^^ BAnnotationElement_Short
    case 'Z' => u2 ^^ BAnnotationElement_Boolean
    case 's' => u2 ^^ BAnnotationElement_String
    case 'e' => for (t <- u2; c <- u2) yield BAnnotationElement_Enum(t, c)
    case 'c' => u2 ^^ BAnnotationElement_Class
    case '@' => annotation ^^ BAnnotationElement_Annotation
    case '[' => list(annotationElement) ^^ BAnnotationElement_Array
    case tag => failure(InvalidClassFileException("wrong annotation tag: " + tag))
  })
}
