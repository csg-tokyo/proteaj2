package phenan.prj.decl

import phenan.prj.exception.ParseException

sealed trait SourceToken {
  def line: Int
  def is (sym: Int): Boolean = false
  def is (id: String): Boolean = false
  def eof: Boolean = false
}

case class IdentifierToken (id: String, line: Int) extends SourceToken {
  override def is (id: String): Boolean = this.id == id
}

case class CharLitToken (lit: Int, line: Int) extends SourceToken

case class StringLitToken (lit: String, line: Int) extends SourceToken

case class SymbolToken (ch: Int, line: Int) extends SourceToken {
  override def is (sym: Int): Boolean = this.ch == sym
  override def toString: String = new String(Character.toChars(ch))
}

case class Whitespaces (ws: String, line: Int) extends SourceToken

case object EndOfSource extends SourceToken {
  override def line: Int = -1
  override def eof: Boolean = true
}

case class InvalidToken (e: ParseException, line: Int) extends SourceToken

case class Snippet (src: String, line: Int)
