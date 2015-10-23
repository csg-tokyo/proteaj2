package phenan.prj.body

import phenan.prj.combinator.ScannerlessParsers
import scala.language.implicitConversions
import scalaz.Memo._

trait CommonParsers extends ScannerlessParsers {
  lazy val delimiter: LParser[Any] = elem("white space", Character.isWhitespace).*

  lazy val emptyBrackets = emptyBracket.* ^^ { _.size }
  lazy val emptyBracket = '[' ~> ']'

  lazy val qualifiedName = identifier.+('.')

  lazy val identifier = (elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).*).^ ^^ {
    case s ~ ps => (s :: ps).mkString
  }

  def word (cs: String): LParser[String] = word_cached(cs)
  def regex (r: String): LParser[String] = regex_cached(r)

  implicit def keyword (kw: String): HParser[String] = (word(kw) <~ elem("identifier part", Character.isJavaIdentifierPart).!).^
  implicit def symbol (ch: Char): HParser[Char] = elem(ch).^

  private lazy val word_cached: String => LParser[String] = mutableHashMapMemo { cs => cs.foldRight(LParser.success(cs)) { (ch, r) => elem(ch) ~> r } }
  private lazy val regex_cached: String => LParser[String] = mutableHashMapMemo { s => regularExpression(s.r) }
}
