package phenan.prj.body

import scala.language.implicitConversions

trait CommonParsersModule {
  this: ContextSensitiveParsersModule =>

  trait CommonParsers {
    this: ContextSensitiveParsers =>

    lazy val delimiter: ContextFreeScanner[Any] = elem("white space", Character.isWhitespace).*

    lazy val emptyBrackets: ContextFreeParser[Int] = emptyBracket.* ^^ { _.size }
    lazy val emptyBracket: ContextFreeParser[Char] = '[' ~> ']'

    lazy val qualifiedName: ContextFreeParser[List[String]] = identifier.+('.')

    lazy val identifier: ContextFreeParser[String] = (elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).*).^ ^^ {
      case s ~ ps => (s :: ps).mkString
    }

    implicit def keyword (kw: String): ContextFreeParser[String] = (word(kw) <~ elem("identifier part", Character.isJavaIdentifierPart).!).^

    implicit def symbol (ch: Char): ContextFreeParser[Char] = elem(ch).^
  }
}