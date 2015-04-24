package phenan.prj.combinator

import java.io._

import phenan.prj.exception.ParseException

import scala.collection.immutable.PagedSeq
import scala.util.{Success => Succ, Failure => Fail, _}
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{Reader => In, _}

import scalaz.Memo._

import scala.language.implicitConversions

@Deprecated
trait JParsers {
  type ~ [+A, +B] = JParsersImpl.~[A, B]

  val ~ = JParsersImpl.~

  def hexDigit = JParsersImpl.hexDigit

  def identifier = JParsersImpl.identifier

  def position = JParsersImpl.position

  def element (kind: String, f: Char => Boolean) = JParsersImpl.element(kind, f)

  implicit def symbol (sym: Char): JLexer[Char] = JParsersImpl.symbol(sym)

  implicit def keyword (word: String): JParser[String] = JParsersImpl.keyword(word)

  trait JLexer[+T] {
    def ^ : JParser[T]

    def ~ [U] (parser: => JLexer[U]): JLexer[T ~ U]
    def ~> [U] (parser: => JLexer[U]): JLexer[U]
    def <~ [U] (parser: => JLexer[U]): JLexer[T]

    def | [U >: T] (parser: => JLexer[U]): JLexer[U]

    def ? : JLexer[Option[T]]
    def * : JLexer[List[T]]
    def + : JLexer[List[T]]

    def * [U] (sep: => JLexer[U]): JLexer[List[T]]
    def + [U] (sep: => JLexer[U]): JLexer[List[T]]

    def map [R] (f: T => R): JLexer[R]
    def flatMap [R] (f: T => JLexer[R]): JLexer[R]

    def into [R] (f: T => JLexer[R]): JLexer[R] = flatMap(f)

    def ^^ [R] (f: T => R): JLexer[R] = map(f)
    def >> [R] (f: T => JLexer[R]): JLexer[R] = flatMap(f)

    protected[JParsers] def parser: JParsersImpl.PackratParser[T]
  }

  trait JParser[+T] {
    def ~ [U] (parser: => JParser[U]): JParser[T ~ U]
    def ~> [U] (parser: => JParser[U]): JParser[U]
    def <~ [U] (parser: => JParser[U]): JParser[T]

    def | [U >: T] (parser: => JParser[U]): JParser[U]

    def ? : JParser[Option[T]]
    def ! : JParser[Unit]
    def * : JParser[List[T]]
    def + : JParser[List[T]]

    def * [U] (sep: => JParser[U]): JParser[List[T]]
    def + [U] (sep: => JParser[U]): JParser[List[T]]

    def map [R] (f: T => R): JParser[R]
    def flatMap [R] (f: T => JParser[R]): JParser[R]

    def into [R] (f: T => JParser[R]): JParser[R] = flatMap(f)

    def ^^ [R] (f: T => R): JParser[R] = map(f)
    def ^^^ [R] (f: => R): JParser[R]
    def >> [R] (f: T => JParser[R]): JParser[R] = flatMap(f)

    protected[JParsers] def parser: JParsersImpl.PackratParser[T]
    protected[JParsers] def parse (in: In[Char], fileName: String): Try[T]
  }

  def parse [T] (file: String)(parser: JParser[T]): Try[T] = parse(PagedSeq.fromFile(file), file)(parser)
  def parse [T] (file: File)(parser: JParser[T]): Try[T] = parse(PagedSeq.fromFile(file), file.getName)(parser)
  def parse [T] (source: String, sourceName: String)(parser: JParser[T]): Try[T] = parser.parse(new CharSequenceReader(source), sourceName)
  def parse [T] (reader: Reader, fileName: String)(parser: JParser[T]): Try[T] = parse(PagedSeq.fromReader(reader), fileName)(parser)
  def parse [T] (seq: PagedSeq[Char], fileName: String)(parser: JParser[T]): Try[T] = parser.parse(new PagedSeqReader(seq), fileName)

  object JParsersImpl extends PackratParsers {
    override type Elem = Char

    private lazy val whitespace = elem("white space", Character.isWhitespace)
    private lazy val delimiter = (whitespace.* | lineComment | blockComment).*

    private lazy val idStart = elem("identifier start", Character.isJavaIdentifierStart)
    private lazy val idPart = elem("identifier part", Character.isJavaIdentifierPart)

    private lazy val lineComment = elem('/') ~> elem('/') ~> elem("line comment element", { _ != '\n' }).*
    private lazy val blockComment = elem('/') ~> elem('*') ~> ( elem("block comment element", { _ != '*' }) | elem('*') <~ not(elem('/')) ).* <~ elem('*') <~ elem('/')

    lazy val identifier: JLexer[String] = JLexerImpl(idStart ~ idPart.* ^^ { case s ~ ps => s +: ps.mkString })

    lazy val whitespaces: JLexer[String] = JLexerImpl(whitespace.* ^^ { _.mkString })

    lazy val comment = JLexerImpl(lineComment | blockComment)

    lazy val hexDigit: JLexer[Int] = JLexerImpl(elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ { Character.digit(_, 16) })

    lazy val position: JLexer[Position] = JLexerImpl(new Parser[Position] {
      override def apply(in: Input): ParseResult[Position] = Success(in.pos, in)
    })

    val keyword: String => JParser[String] = mutableHashMapMemo { word =>
      if (Character.isJavaIdentifierStart(word.head)) JParserImpl(word.foldLeft(success(word)) { _ <~ elem(_) } <~ not(idPart))
      else JParserImpl(word.foldLeft(success(word)) { _ <~ elem(_) })
    }

    val symbol: Char => JLexer[Char] = mutableHashMapMemo { sym => JLexerImpl(elem(sym)) }

    def element (kind: String, f: Char => Boolean) = JLexerImpl(elem(kind, f))

    case class JLexerImpl[+T] (parser: PackratParser[T]) extends JLexer[T] {
      def ~ [U] (p: => JLexer[U]): JLexer[T ~ U] = JLexerImpl(parser ~ p.parser)
      def ~> [U] (p: => JLexer[U]): JLexer[U] = JLexerImpl(parser ~> p.parser)
      def <~ [U] (p: => JLexer[U]): JLexer[T] = JLexerImpl(parser <~ p.parser)

      def | [U >: T] (p: => JLexer[U]): JLexer[U] = JLexerImpl(parser | p.parser)

      def ? : JLexer[Option[T]] = JLexerImpl(parser.?)
      def * : JLexer[List[T]] = JLexerImpl(rep(parser))
      def + : JLexer[List[T]] = JLexerImpl(rep1(parser))

      def * [U] (sep: => JLexer[U]): JLexer[List[T]] = JLexerImpl(repsep(parser, sep.parser))
      def + [U] (sep: => JLexer[U]): JLexer[List[T]] = JLexerImpl(rep1sep(parser, sep.parser))

      def map [R] (f: T => R): JLexer[R] = JLexerImpl(parser ^^ f)
      def flatMap [R] (f: T => JLexer[R]): JLexer[R] = JLexerImpl(parser.flatMap(f(_).parser))

      lazy val ^ : JParser[T] = JParserImpl(parser)
    }

    case class JParserImpl[+T] (parser: PackratParser[T]) extends JParser[T] {
      def ~ [U] (p: => JParser[U]): JParser[T ~ U] = JParserImpl((parser <~ delimiter) ~ p.parser)
      def ~> [U] (p: => JParser[U]): JParser[U] = JParserImpl((parser <~ delimiter) ~> p.parser)
      def <~ [U] (p: => JParser[U]): JParser[T] = JParserImpl(parser <~ delimiter <~ p.parser)

      def | [U >: T] (p: => JParser[U]): JParser[U] = JParserImpl(parser | p.parser)

      def ? : JParser[Option[T]] = JParserImpl(parser.?)
      def ! : JParser[Unit] = JParserImpl(not(parser))
      def * : JParser[List[T]] = JParserImpl(repsep(parser, delimiter))
      def + : JParser[List[T]] = JParserImpl(rep1sep(parser, delimiter))

      def * [U] (sep: => JParser[U]): JParser[List[T]] = JParserImpl(repsep(parser, delimiter <~ sep.parser <~ delimiter))
      def + [U] (sep: => JParser[U]): JParser[List[T]] = JParserImpl(rep1sep(parser, delimiter <~ sep.parser <~ delimiter))

      def map [R] (f: T => R): JParser[R] = JParserImpl(parser ^^ f)
      def flatMap [R] (f: T => JParser[R]): JParser[R] = JParserImpl(parser.flatMap(f(_).parser))

      def ^^^ [R] (f: => R): JParser[R] = JParserImpl(parser ^^^ f)

      override protected[JParsers] def parse (in: In[Char], fileName: String): Try[T] = ((delimiter ~> parser) <~ delimiter).apply(new PackratReader[Char](in)) match {
        case Success(result, _) => Succ(result)
        case NoSuccess(msg, _)  => Fail(ParseException("parse error in " + fileName + " : " + msg))
      }
    }
  }
}
