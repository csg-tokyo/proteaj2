package phenan.prj.decl

import java.io._
import phenan.prj.state.JState

import scala.util._

import Character._

class SourceReader private (in: Reader, var ch: Int, state: JState) {
  def head: Option[SourceToken] = ???
  def nextToken: Option[SourceToken] = ???
  def nextBlock: Option[SourceBlock] = ???

  private def readNext(): SourceToken = try {
    ch match {
      case -1                            => EndOfSource
      case s if isJavaIdentifierStart(s) => readNextIdentifier(new StringBuilder + s.toChar)
      case w if isWhitespace(w)          => ch = in.read(); readNext()
      case '\'' => readCharLiteral()
      case '\"' => ???
      case '/'  => in.read() match {
        case '/' => readLineComment(); readNext()
        case '*' => readBlockComment(); readNext()
        case c   => ch = c; SymbolToken('/')
      }
      case c => ch = in.read(); SymbolToken(c.toChar)


    }
  } catch {
    case e : IOException =>
      ch = -1
      state.error("io error", e)
      InvalidToken
  }

  private def readNextIdentifier(buf: StringBuilder): IdentifierToken = {
    ch = in.read()
    if (isJavaIdentifierPart(ch)) readNextIdentifier(buf + ch.toChar)
    else IdentifierToken(buf.toString())
  }

  private def readCharLiteral(): SourceToken = {
    ch = in.read()
    if (ch == -1 || ch == '\n' || ch == '\'') {
      ch = in.read()
      state.error("invalid character literal")
      InvalidToken
    }
    else {
      val lit = if (ch == '\\') readEscapedChar() else ch.toChar.toString

      ch = in.read()
      if (ch == '\'') {
        ch = in.read()
        CharLitToken(lit)
      }
      else {
        state.error("invalid character literal : expected single quote but found " + ch)
        InvalidToken
      }
    }
  }

  private def readEscapedChar(): String = {
    in.read() match {
      case 'b'  => "\\b"
      case 'f'  => "\\f"
      case 'n'  => "\\n"
      case 'r'  => "\\r"
      case 't'  => "\\t"
      case 'u'  => readCharCode16()
      case '\'' => "\\\'"
      case '\"' => "\\\""
      case '\\' => "\\\\"
      case n if '0' <= n && n <= '7' => readCharCode8()
      case n =>
        state.error("invalid escape sequence")
        ""
    }
  }

  private def readCharCode8(): String = {
    ???
  }

  private def readCharCode16(): String = {
    ???
  }

  private def readLineComment(): Unit = ???
  private def readBlockComment(): Unit = ???

  private var next: SourceToken = readNext()
}

object SourceReader {
  def apply (file: File): Try[SourceReader] = {
    ???
  }
}