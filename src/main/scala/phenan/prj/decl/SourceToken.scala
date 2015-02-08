package phenan.prj.decl

sealed trait SourceToken

case class IdentifierToken (id: String) extends SourceToken

case class CharLitToken (lit: String) extends SourceToken

case class StringLitToken (lit: String) extends SourceToken

case class SymbolToken (ch: Char) extends SourceToken

case object EndOfSource extends SourceToken

case object InvalidToken extends SourceToken

class SourceBlock


