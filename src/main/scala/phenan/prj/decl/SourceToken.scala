package phenan.prj.decl

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
}

case class Whitespaces (ws: String, line: Int) extends SourceToken

case object EndOfSource extends SourceToken {
  override def line: Int = -1
  override def eof: Boolean = true
}

case class InvalidToken (line: Int) extends SourceToken
