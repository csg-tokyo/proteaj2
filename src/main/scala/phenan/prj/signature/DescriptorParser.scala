package phenan.prj.signature

import phenan.prj._

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

trait DescriptorParser {
  this: Application =>

  object DescriptorParsers extends PackratParsers {
    override type Elem = Char

    lazy val methodDesc: PackratParser[JMethodSignature] =
      ('(' ~> typeDesc.* <~ ')') ~ returnDesc ^^ { case paramTypes ~ retType => JMethodSignature(Nil, paramTypes.map(sig => JParameterSignature(Nil, sig, false, None, Nil)), retType, Nil, Nil, Nil, Nil, Nil) }

    lazy val typeDesc: PackratParser[JTypeSignature] = baseDesc | objectDesc | arrayDesc

    lazy val returnDesc: PackratParser[JTypeSignature] = typeDesc | 'V' ^^^ VoidTypeSignature

    lazy val arrayDesc: PackratParser[JArrayTypeSignature] = '[' ~> typeDesc ^^ JArrayTypeSignature

    lazy val baseDesc: PackratParser[JPrimitiveTypeSignature] =
      'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
        'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature | 'J' ^^^ LongTypeSignature |
        'S' ^^^ ShortTypeSignature | 'Z' ^^^ BoolTypeSignature

    lazy val objectDesc: PackratParser[SimpleClassTypeSignature] =
      'L' ~> className <~ ';' ^^ { name => SimpleClassTypeSignature(name, Nil) }

    lazy val className: Parser[String] = repsep(identifier, '/') ^^ { ids => ids.mkString("/") }

    lazy val identifier: PackratParser[String] =
      elem("id start", Character.isJavaIdentifierStart) ~ elem("id part", Character.isJavaIdentifierPart).* ^^ {
        case s ~ ps => (s :: ps).mkString
      }

    def parse[T](desc: String, kind: String, parser: Parser[T]): Option[T] = parser(new PackratReader[Char](new CharSequenceReader(desc))) match {
      case Success(ret, _) => Some(ret)
      case NoSuccess(msg, _) => errorAndReturn("fail to parse " + kind + " : " + desc + "\n" + msg, None)
    }
  }

  import DescriptorParsers._

  def parseMethodDescriptor(desc: String): Option[JMethodSignature] = parse(desc, "method descriptor", methodDesc)
  def parseFieldDescriptor(desc: String): Option[JTypeSignature] = parse(desc, "field descriptor", typeDesc)
  def parseArrayDescriptor(desc: String): Option[JArrayTypeSignature] = parse(desc, "array type descriptor", arrayDesc)
  def parseReturnDescriptor(desc: String): Option[JTypeSignature] = parse(desc, "return descriptor", returnDesc)
  def parseClassTypeDescriptor(desc: String): Option[JClassTypeSignature] = parse(desc, "class type descriptor", objectDesc)
}