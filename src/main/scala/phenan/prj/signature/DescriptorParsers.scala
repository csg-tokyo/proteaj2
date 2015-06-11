package phenan.prj.signature

import phenan.prj._
import phenan.prj.state.JState

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

object DescriptorParsers extends PackratParsers {
  override type Elem = Char

  def parseMethodDescriptor (desc: String)(implicit state: JState): Option[JMethodSignature] = parse(desc, "method descriptor", methodDesc)

  def parseFieldDescriptor (desc: String)(implicit state: JState): Option[JTypeSignature] = parse(desc, "field descriptor", typeDesc)

  def parseArrayDescriptor (desc: String)(implicit state: JState): Option[JArrayTypeSignature] = parse(desc, "array type descriptor", arrayDesc)

  def parseReturnDescriptor (desc: String)(implicit state: JState): Option[JTypeSignature] = parse(desc, "return descriptor", returnDesc)

  private lazy val methodDesc: PackratParser[JMethodSignature] =
    ( '(' ~> typeDesc.* <~ ')' ) ~ returnDesc ^^ { case paramTypes ~ retType => JMethodSignature(Nil, paramTypes.map(sig => JParameterSignature(Nil, sig, None, false, None)), retType, Nil, Nil, Nil, Nil) }

  private lazy val typeDesc: PackratParser[JTypeSignature] = baseDesc | objectDesc | arrayDesc

  private lazy val returnDesc: PackratParser[JTypeSignature] = typeDesc | 'V' ^^^ VoidTypeSignature

  private lazy val arrayDesc: PackratParser[JArrayTypeSignature] = '[' ~> typeDesc ^^ JArrayTypeSignature

  private lazy val baseDesc: PackratParser[JPrimitiveTypeSignature] =
    'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
      'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature  | 'J' ^^^ LongTypeSignature |
      'S' ^^^ ShortTypeSignature  | 'Z' ^^^ BoolTypeSignature

  private lazy val objectDesc: PackratParser[SimpleClassTypeSignature] =
    'L' ~> className <~ ';' ^^ { name => SimpleClassTypeSignature(name, Nil) }

  private lazy val className = repsep(identifier, '/') ^^ { ids => ids.mkString("/") }

  private lazy val identifier: PackratParser[String] =
    elem("id start", Character.isJavaIdentifierStart) ~ elem("id part", Character.isJavaIdentifierPart).* ^^ {
      case s ~ ps => (s :: ps).mkString
    }

  private def parse[T] (desc: String, kind: String, parser: Parser[T])(implicit state: JState): Option[T] = parser(new PackratReader[Char](new CharSequenceReader(desc))) match {
    case Success(ret, _)   => Some(ret)
    case NoSuccess(msg, _) => state.errorAndReturn("fail to parse " + kind + " : " + desc + "\n" + msg, None)
  }
}
