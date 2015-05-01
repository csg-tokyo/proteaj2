package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

object DescriptorParsers extends PackratParsers {
  override type Elem = Char

  def parseMethodDescriptor (desc: String)(implicit state: JState): Option[JMethodSignature] = parse(desc, "method descriptor", methodDesc)

  def parseFieldDescriptor (desc: String)(implicit state: JState): Option[JTypeSignature] = parse(desc, "field descriptor", typeDesc)

  def parseArrayDescriptor (desc: String)(implicit state: JState): Option[JArrayTypeSignature] = parse(desc, "array type descriptor", arrayDesc)

  private def parse[T] (desc: String, kind: String, parser: Parser[T])(implicit state: JState): Option[T] = parser(new PackratReader[Char](new CharSequenceReader(desc))) match {
    case Success(ret, _)   => Some(ret)
    case NoSuccess(msg, _) =>
      state.error("fail to parse " + kind + " : " + desc + "\n" + msg)
      None
  }

  protected lazy val methodDesc: PackratParser[JMethodSignature] =
    ( '(' ~> typeDesc.* <~ ')' ) ~ returnDesc ^^ { case paramTypes ~ retType => JMethodSignature(Nil, paramTypes.map(sig => JParameterSignature(Nil, sig, None, false, None)), retType, Nil, Nil, Nil, Nil) }

  protected lazy val typeDesc: PackratParser[JTypeSignature] = baseDesc | objectDesc | arrayDesc

  protected lazy val returnDesc: PackratParser[JTypeSignature] = typeDesc | 'V' ^^^ VoidTypeSignature

  protected lazy val arrayDesc: PackratParser[JArrayTypeSignature] = '[' ~> typeDesc ^^ JArrayTypeSignature

  protected lazy val baseDesc: PackratParser[JPrimitiveTypeSignature] =
    'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
      'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature  | 'J' ^^^ LongTypeSignature |
      'S' ^^^ ShortTypeSignature  | 'Z' ^^^ BoolTypeSignature

  protected lazy val objectDesc: PackratParser[SimpleClassTypeSignature] =
    'L' ~> className <~ ';' ^^ { name => SimpleClassTypeSignature(name, Nil) }

  protected lazy val className = repsep(identifier, '/') ^^ { ids => ids.mkString("/") }

  protected lazy val identifier: PackratParser[String] =
    elem("id start", Character.isJavaIdentifierStart) ~ elem("id part", Character.isJavaIdentifierPart).* ^^ {
      case s ~ ps => (s :: ps).mkString
    }
}
