package phenan.prj.signature

import phenan.prj._
import phenan.prj.state.JState

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

import scala.language.implicitConversions

object BinaryNameParsers extends PackratParsers {
  override type Elem = Char

  def parseBinaryName (name: String)(implicit state: JState): Option[JTypeSignature] = parse(name, binaryName)

  def parseBinaryClassName (name: String)(implicit state: JState): Option[JClassTypeSignature] = parse(name, className)

  private lazy val binaryName: PackratParser[JTypeSignature] = arrayName | primitiveName | className

  private lazy val className: PackratParser[JClassTypeSignature] = qualifiedName ^^ { name =>
    SimpleClassTypeSignature(name.mkString("/"), Nil)
  }

  private lazy val primitiveName: PackratParser[JPrimitiveTypeSignature] =
    "byte" ^^^ ByteTypeSignature | "char" ^^^ CharTypeSignature | "double" ^^^ DoubleTypeSignature |
      "float" ^^^ FloatTypeSignature | "int" ^^^ IntTypeSignature  | "long" ^^^ LongTypeSignature |
      "short" ^^^ ShortTypeSignature  | "boolean" ^^^ BoolTypeSignature | "void" ^^^ VoidTypeSignature

  private lazy val arrayName: PackratParser[JArrayTypeSignature] = '[' ~> componentName ^^ JArrayTypeSignature

  private lazy val componentName: PackratParser[JTypeSignature] = 'L' ~> className <~ ';' | baseName

  private lazy val baseName: PackratParser[JPrimitiveTypeSignature] =
    'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
      'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature  | 'J' ^^^ LongTypeSignature |
      'S' ^^^ ShortTypeSignature  | 'Z' ^^^ BoolTypeSignature

  private def parse[T] (desc: String, parser: Parser[T])(implicit state: JState): Option[T] = parser(new PackratReader[Char](new CharSequenceReader(desc))) match {
    case Success(ret, _)   => Some(ret)
    case NoSuccess(msg, _) => state.errorAndReturn("fail to parse binary name : " + desc + "\n" + msg, None)
  }

  private lazy val qualifiedName = rep1sep(identifier, '.')

  private lazy val identifier: PackratParser[String] = (elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).*) ^^ {
    case s ~ ps => (s :: ps).mkString
  }

  private implicit def keyword (kw: String): PackratParser[String] = kw.foldRight(success(kw)) { (ch, r) => elem(ch) ~> r }
}
