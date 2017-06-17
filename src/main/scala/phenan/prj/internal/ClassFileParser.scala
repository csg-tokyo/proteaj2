package phenan.prj.internal

import java.io._

import phenan.prj.Application
import phenan.prj.combinator._
import phenan.prj.exception._

import scala.util._

trait ClassFileParser {
  this: Application =>

  object ClassFileParsers extends ByteParsers {

    def parseAttribute [T] (attr: BAttributeInfo)(parser: ByteParser[T]): Option[T] = parse(attr.data)(parser) match {
      case Success(result) => Some(result)
      case Failure(e) =>
        error("broken class file : cannot parse attribute", e)
        None
    }

    lazy val classFile: ByteParser[BClassFile] = for {
      magic <- s4 if magic == 0xcafebabe
      minor <- u2
      major <- u2
      cPool <- constantPool
      flags <- u2
      cls   <- u2
      sup   <- u2
      ifs   <- list(u2)
      fs    <- list(field)
      ms    <- list(method)
      attrs <- list(attribute)
    } yield BClassFile(minor, major, cPool, flags, cls, sup, ifs, fs, ms, attrs)

    lazy val constantPool: ByteParser[BConstantPool] = for {
      max <- u2
      map <- constantMap(1, max)
    } yield BConstantPool(map)

    private def constantMap (index: Int, max: Int): ByteParser[Map[Int, BConstant]] = {
      if (index < max) for {
        c <- constant
        m <- constantMap(getNextIndex(index, c), max)
      } yield m + (index -> c)
      else ret(Map.empty)
    }

    private def getNextIndex (index: Int, c: BConstant) = c match {
      case BLongValue(_) | BDoubleValue(_) => index + 2
      case _ => index + 1
    }

    lazy val constant: ByteParser[BConstant] = u1 >>= {
      case 0x01 => for (value <- utf) yield BUtf8Value(value)
      case 0x03 => for (value <- s4)  yield BIntValue(value)
      case 0x04 => for (value <- f4)  yield BFloatValue(value)
      case 0x05 => for (value <- s8)  yield BLongValue(value)
      case 0x06 => for (value <- f8)  yield BDoubleValue(value)
      case 0x07 => for (ref <- u2)    yield BClassRef(ref)
      case 0x08 => for (ref <- u2)    yield BStringRef(ref)
      case 0x09 => for (cls <- u2; sig <- u2) yield BFieldRef(cls, sig)
      case 0x0a => for (cls <- u2; sig <- u2) yield BMethodRef(cls, sig)
      case 0x0b => for (cls <- u2; sig <- u2) yield BIMethodRef(cls, sig)
      case 0x0c => for (nam <- u2; typ <- u2) yield BNameTypeRef(nam, typ)
      case 0x0f => for (knd <- u1; ind <- u2) yield BMethodHandleInfo(knd, ind)
      case 0x10 => for (dsc <- u2)            yield BMethodTypeInfo(dsc)
      case 0x12 => for (bst <- u2; sig <- u2) yield BInvokeDynamicInfo(bst, sig)
      case n    => failure(InvalidClassFileException("wrong constant tag: " + n))
    }

    lazy val field: ByteParser[BField] = for {
      mod  <- u2
      name <- u2
      desc <- u2
      attr <- list(attribute)
    } yield BField(mod, name, desc, attr)

    lazy val method: ByteParser[BMethod] = for {
      mod  <- u2
      name <- u2
      desc <- u2
      attr <- list(attribute)
    } yield BMethod(mod, name, desc, attr)

    lazy val attribute: ByteParser[BAttributeInfo] = for {
      tag <- u2
      len <- s4
      dat <- bytes(len)
    } yield BAttributeInfo(tag, dat)

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

  import ClassFileParsers._

  def parseClassFile (stream: InputStream): Try[BClassFile] = parse(stream)(classFile)

  def parseInnerClassesAttribute (attr: BAttributeInfo): Option[InnerClassesAttribute] = parseAttribute(attr)(innerClasses)
  def parseSignatureAttribute (attr: BAttributeInfo): Option[SignatureAttribute] = parseAttribute(attr)(signature)
  def parseExceptionsAttribute (attr: BAttributeInfo): Option[ExceptionsAttribute] = parseAttribute(attr)(exceptions)
  def parseRuntimeVisibleAnnotationsAttribute (attr: BAttributeInfo): Option[RuntimeVisibleAnnotationsAttribute] = parseAttribute(attr)(runtimeVisibleAnnotations)

}
