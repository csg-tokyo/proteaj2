package phenan.prj.decl

import java.io._
import java.lang.{StringBuilder => SBuilder}
import java.util.function.IntConsumer
import phenan.prj.exception.ParseException
import phenan.prj.state.JState

import scala.util._

import Character._

class SourceReader private (private val in: Reader, private var current: Int)(implicit state: JState) {
  def head: SourceToken = look(0)

  def look (n: Int): SourceToken = {
    fetch(n)
    look(n, queue)
  }

  def next: SourceToken = {
    fetch(0)
    discard(0)
  }

  def next (n: Int): SourceToken = {
    fetch(n)
    discard(n)
  }

  def nextBlock: Try[String] = ???

  def nextExpression: Try[Snippet] = {
    val l = line
    readUntilEndOfExpr(new SBuilder, 0).map(Snippet(_, l))
  }

  def skipUntil (f: SourceToken => Boolean): SourceToken = {
    while (! eof && ! f(head)) next
    next
  }

  def skipBlock (p: Int): Unit = {
    while (! eof && braces.nonEmpty && braces.head >= p) next
    next
  }

  def position: Int = pos

  def eof: Boolean = head.eof

  /* for look-ahead */

  private def fetch (n: Int): Unit = {
    if (nFetched <= n) readNext() match {
      case Some(ws: Whitespaces) =>
        queue = queue :+ ws
        fetch(n)
      case Some(token) =>
        queue = queue :+ token
        nFetched += 1
        fetch(n)
      case None => // do nothing
    }
  }

  private def look (n: Int, q: List[SourceToken]): SourceToken = {
    if (q.nonEmpty) q.head match {
      case _: Whitespaces => look(n, q.tail)
      case _ if n > 0 => look(n - 1, q.tail)
      case token => token
    }
    else EndOfSource
  }

  private def discard (n: Int): SourceToken = {
    if (queue.nonEmpty) queue.head match {
      case EndOfSource => EndOfSource
      case _: Whitespaces =>
        queue = queue.tail
        discard(n)
      case _ if n > 0 =>
        queue = queue.tail
        nFetched -= 1
        discard(n - 1)
      case token =>
        queue = queue.tail
        nFetched -= 1
        token
    }
    else EndOfSource
  }

  private def readUntilEndOfExpr (buf: SBuilder, pars: Int): Try[String] = {
    if (queue.nonEmpty) queue.head match {
      case s @ SymbolToken('(' | '{', _) =>
        queue = queue.tail
        nFetched -= 1
        readUntilEndOfExpr(buf.appendCodePoint(s.ch), pars + 1)
      case s @ SymbolToken(')' | '}', _) if pars > 0 =>
        queue = queue.tail
        nFetched -= 1
        readUntilEndOfExpr(buf.appendCodePoint(s.ch), pars - 1)
      case SymbolToken(')' | '}' | ',' | ';', _) =>
        Success(buf.toString)
      case InvalidToken(_) =>
        Failure(ParseException("invalid token"))
      case Whitespaces(ws, _) =>
        queue = queue.tail
        readUntilEndOfExpr(buf.append(ws), pars)
      case token =>
        queue = queue.tail
        nFetched -= 1
        readUntilEndOfExpr(appendToken(buf, token), pars)
    }
    else readNext() match {
      case Some(s @ SymbolToken('(' | '{', _)) =>
        readUntilEndOfExpr(buf.appendCodePoint(s.ch), pars + 1)
      case Some(s @ SymbolToken(')' | '}', _)) if pars > 0 =>
        readUntilEndOfExpr(buf.appendCodePoint(s.ch), pars - 1)
      case Some(s @ SymbolToken(')' | '}' | ',' | ';', _)) =>
        queue = queue :+ s
        nFetched += 1
        Success(buf.toString)
      case Some(InvalidToken(_)) =>
        Failure(ParseException("invalid token"))
      case Some(token) =>
        readUntilEndOfExpr(appendToken(buf, token), pars)
      case None =>
        Failure(ParseException("end of expression is not found"))
    }
  }

  private def appendToken (buf: SBuilder, token: SourceToken): SBuilder = token match {
    case IdentifierToken(id, _) => buf.append(id)
    case CharLitToken(lit, _)   =>
      buf.append('\'')
      appendLitChar(buf, lit)
      buf.append('\'')
    case StringLitToken(lit, _) =>
      buf.append('\"')
      lit.codePoints().forEach(codePointAppender(buf))
      buf.append('\"')
    case SymbolToken(ch, _)     => buf.appendCodePoint(ch)
    case Whitespaces(ws, _)     => buf.append(ws)
    case _                      => buf.append("¿")
  }

  private def codePointAppender(buf: SBuilder) = new IntConsumer {
    override def accept(value: Int): Unit = appendLitChar(buf, value)
  }

  private def appendLitChar (buf: SBuilder, codePoint: Int): SBuilder = codePoint match {
    case '\b' => buf.append('\\').append('b')
    case '\f' => buf.append('\\').append('f')
    case '\n' => buf.append('\\').append('n')
    case '\r' => buf.append('\\').append('r')
    case '\t' => buf.append('\\').append('t')
    case '\'' => buf.append('\\').append('\'')
    case '\"' => buf.append('\\').append('\"')
    case '\\' => buf.append('\\').append('\\')
    case ch   => buf.appendCodePoint(ch)
  }

  private var nFetched: Int = 0
  private var queue: List[SourceToken] = Nil

  /* tokenization method */

  private def readNext(): Option[SourceToken] = try {
    current match {
      case -1                            => None
      case s if isJavaIdentifierStart(s) => Some(readIdentifier((new SBuilder).appendCodePoint(s)))
      case w if isWhitespace(w) => Some(readWhitespaces(w))
      case '\'' => Some(readCharLiteral())
      case '\"' => Some(readStringLiteral())
      case '/'  => read() match {
        case '/' => readLineComment(); readNext()
        case '*' => readBlockComment(); readNext()
        case c   => Some(SymbolToken('/', line))
      }
      case c => Some(readSymbol())
    }
  } catch { case e : IOException =>
    current = -1
    state.error("io error", e)
    None
  }

  private def readIdentifier(buf: SBuilder): IdentifierToken = {
    read()
    if (isJavaIdentifierPart(current)) readIdentifier(buf.appendCodePoint(current))
    else IdentifierToken(buf.toString, line)
  }

  private def readWhitespaces(w: Int): Whitespaces = {
    if (w == '\n') line += 1
    readWhitespaces((new SBuilder).appendCodePoint(w))
  }

  private def readWhitespaces(buf: SBuilder): Whitespaces = read() match {
    case '\n' =>
      line += 1
      readWhitespaces(buf.appendCodePoint('\n'))
    case w if isWhitespace(w) => readWhitespaces(buf.appendCodePoint(w))
    case _ => Whitespaces(buf.toString, line)
  }

  private def readCharLiteral(): SourceToken = {
    read()
    if (current == -1 || current == '\n' || current == '\'') {
      read()
      state.error("invalid character literal")
      InvalidToken(line)
    }
    else {
      val lit = current match {
        case '\\' => readEscapedChar()
        case c    => read(); c
      }

      if (current == '\'') {
        read()
        CharLitToken(lit, line)
      }
      else {
        state.error("invalid character literal : expected single quote but found " + current)
        InvalidToken(line)
      }
    }
  }

  private def readStringLiteral(): SourceToken = {
    read()
    readStringLiteral(new SBuilder)
  }

  private def readStringLiteral(buf: SBuilder): SourceToken = current match {
    case '\"'      =>
      read()
      StringLitToken(buf.toString, line)
    case '\n' | -1 =>
      read()
      state.error("invalid string literal")
      InvalidToken(line)
    case '\\'      =>
      readStringLiteral(buf.appendCodePoint(readEscapedChar()))
    case ch        =>
      read()
      readStringLiteral(buf.appendCodePoint(ch))
  }

  private def readEscapedChar(): Int = read() match {
    case 'b'  => read(); '\b'
    case 'f'  => read(); '\f'
    case 'n'  => read(); '\n'
    case 'r'  => read(); '\r'
    case 't'  => read(); '\t'
    case 'u'  => readCharCode16(0, 0)
    case '\'' => read(); '\''
    case '\"' => read(); '\"'
    case '\\' => read(); '\\'
    case n if '0' <= n && n <= '7' => readCharCode8(n)
    case n =>
      state.error("invalid escape sequence")
      0
  }

  private def readCharCode8(n: Int): Int = {
    val code1 = digit(n, 8)
    read()
    if (! ('0' <= current && current <= '7')) code1
    else {
      val code2 = code1 * 8 + digit(current, 8)
      read()
      if (! ('0' <= current && current <= '7')) code2
      else {
        val code3 = code2 * 8 + digit(current, 8)
        read()

        if (0 <= code3 && code3 <= 0x00ff) code3
        else {
          state.error("invalid octal char code")
          code3
        }
      }
    }
  }

  private def readCharCode16(n: Int, code: Int): Int = {
    read()
    if (n > 4) code
    else if ('0' <= current && current <= '9' || 'a' <= current && current <= 'f' || 'A' <= current && current <= 'F') {
      readCharCode16(n + 1, code * 16 + digit(current, 16))
    }
    else {
      state.error("invalid unicode escape")
      code
    }
  }

  private def readLineComment(): Unit = read() match {
    case '\n' => line += 1; read()
    case -1   => read()
    case _    => readLineComment()
  }

  private def readBlockComment(): Unit = read() match {
    case '*'  => readBlockComment_Star()
    case '\n' => line += 1; readBlockComment()
    case -1   => current = -1; state.error("end of comment is not found")
    case _    => readBlockComment()
  }

  private def readBlockComment_Star(): Unit = read() match {
    case '*'  => readBlockComment_Star()
    case '/'  => read()
    case '\n' => line += 1; readBlockComment()
    case -1   => current = -1; state.error("end of comment is not found")
    case _    => readBlockComment()
  }

  private def readSymbol(): SymbolToken = {
    val c = current

    if (c == '{') braces = pos :: braces
    else if (c == '}') braces = braces.tail

    read()
    SymbolToken(c, line)
  }

  private def read(): Int = {
    current = in.read()
    pos += 1
    current
  }

  private var pos: Int = 0
  private var line: Int = 1

  private var braces: List[Int] = Nil
}

object SourceReader {
  def apply (file: File)(implicit state: JState): Try[SourceReader] = Try {
    val in = new BufferedReader(new FileReader(file))
    new SourceReader(in, in.read())
  }

  def apply (file: String)(implicit state: JState): Try[SourceReader] = Try {
    val in = new BufferedReader(new FileReader(file))
    new SourceReader(in, in.read())
  }

  def apply (reader: Reader)(implicit state: JState): Try[SourceReader] = Try {
    val in = new BufferedReader(reader)
    new SourceReader(in, in.read())
  }
}