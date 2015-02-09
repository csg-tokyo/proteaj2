package phenan.prj.decl

import java.io._
import phenan.prj.state.JState

import scala.util._

import Character._

class SourceReader private (in: Reader, private var lookAhead: Int)(implicit state: JState) {
  def head: SourceToken = next
  def nextToken: SourceToken = {
    val n = next
    next = readNext()
    n
  }
  def nextBlock: Option[SourceBlock] = ???

  private def readNext(): SourceToken = try {
    lookAhead match {
      case -1                            => EndOfSource
      case s if isJavaIdentifierStart(s) => readNextIdentifier(new StringBuilder + s.toChar)
      case '\n' => line += 1; lookAhead = in.read(); readNext()
      case w if isWhitespace(w) => lookAhead = in.read(); readNext()
      case '\'' => readCharLiteral()
      case '\"' => readStringLiteral()
      case '/'  => in.read() match {
        case '/' => readLineComment(); readNext()
        case '*' => readBlockComment(); readNext()
        case c   => lookAhead = c; SymbolToken('/')
      }
      case c => lookAhead = in.read(); SymbolToken(c.toChar)
    }
  } catch {
    case e : IOException =>
      lookAhead = -1
      state.error("io error", e)
      InvalidToken
  }

  private def readNextIdentifier(buf: StringBuilder): IdentifierToken = {
    lookAhead = in.read()
    if (isJavaIdentifierPart(lookAhead)) readNextIdentifier(buf + lookAhead.toChar)
    else IdentifierToken(buf.toString())
  }

  private def readCharLiteral(): SourceToken = {
    lookAhead = in.read()
    if (lookAhead == -1 || lookAhead == '\n' || lookAhead == '\'') {
      lookAhead = in.read()
      state.error("invalid character literal")
      InvalidToken
    }
    else {
      val lit = lookAhead match {
        case '\\' => readEscapedChar()
        case c    => lookAhead = in.read(); c.toChar.toString
      }

      if (lookAhead == '\'') {
        lookAhead = in.read()
        CharLitToken(lit)
      }
      else {
        state.error("invalid character literal : expected single quote but found " + lookAhead)
        InvalidToken
      }
    }
  }

  private def readStringLiteral(): SourceToken = {
    lookAhead = in.read()
    readStringLiteral(new StringBuilder)
  }

  private def readStringLiteral(buf: StringBuilder): SourceToken = {
    lookAhead match {
      case '\"'      =>
        lookAhead = in.read()
        StringLitToken(buf.toString())
      case '\n' | -1 =>
        lookAhead = in.read()
        state.error("invalid string literal")
        InvalidToken
      case '\\'      =>
        readStringLiteral(buf.append(readEscapedChar()))
      case ch        =>
        lookAhead = in.read()
        readStringLiteral(buf + ch.toChar)
    }
  }

  private def readEscapedChar(): String = {
    in.read() match {
      case 'b'  => lookAhead = in.read(); "\\b"
      case 'f'  => lookAhead = in.read(); "\\f"
      case 'n'  => lookAhead = in.read(); "\\n"
      case 'r'  => lookAhead = in.read(); "\\r"
      case 't'  => lookAhead = in.read(); "\\t"
      case 'u'  => readCharCode16(0, new StringBuilder + '\\' + 'u')
      case '\'' => lookAhead = in.read(); "\\\'"
      case '\"' => lookAhead = in.read(); "\\\""
      case '\\' => lookAhead = in.read(); "\\\\"
      case n if '0' <= n && n <= '7' => readCharCode8(n)
      case n =>
        state.error("invalid escape sequence")
        ""
    }
  }

  private def readCharCode8(n: Int): String = {
    val buf = new StringBuilder + '\\' + n.toChar
    lookAhead = in.read()
    if (! ('0' <= lookAhead && lookAhead <= '7')) buf.toString()
    else {
      buf.append(lookAhead.toChar)
      lookAhead = in.read()
      if (! ('0' <= lookAhead && lookAhead <= '7')) buf.toString()
      else {
        buf.append(lookAhead.toChar)
        lookAhead = in.read()
        buf.toString()
      }
    }
  }

  private def readCharCode16(n: Int, buf: StringBuilder): String = {
    lookAhead = in.read()
    if (n > 4) buf.toString()
    else if ('0' <= lookAhead && lookAhead <= '9' || 'a' <= lookAhead && lookAhead <= 'f' || 'A' <= lookAhead && lookAhead <= 'F') {
      readCharCode16(n + 1, buf + lookAhead.toChar)
    }
    else {
      state.error("invalid unicode escape")
      ""
    }
  }

  private def readLineComment(): Unit = {
    in.read() match {
      case '\n' => line += 1; lookAhead = in.read()
      case -1   => lookAhead = in.read()
      case _    => readLineComment()
    }
  }

  private def readBlockComment(): Unit = {
    in.read() match {
      case '*'  => in.read() match {
        case '/' => lookAhead = in.read()
        case '\n' => line += 1; readBlockComment()
        case _   => readBlockComment()
      }
      case '\n' => line += 1; readBlockComment()
      case _    => readBlockComment()
    }
  }

  private var next: SourceToken = readNext()
  private var line: Int = 0
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