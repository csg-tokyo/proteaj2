package phenan.prj.declaration

import phenan.prj.util._

import scalaz.Memo._

sealed trait DToken {
  def raw: String
  override def toString = raw
}

class Identifier private (val id: String) extends DToken {
  def raw = id
}

class Symbol private (val symbol: Char) extends DToken {
  def raw = symbol.toString
}

class Whitespace private (ws: Char) extends DToken {
  def raw = ws.toString
}

class CharLiteral private (literal: Char) extends DToken {
  def raw = '\'' + LiteralUtil.escape(literal) + '\''
}

class StrLiteral private (val value: String) extends DToken {
  def raw = '\"' + value.flatMap(LiteralUtil.escape) + '\"'
}

class ErrorToken private (msg: String) extends DToken {
  def raw = "<error>"
}

object Identifier {
  def apply (id: String): Identifier = get(id)
  def unapply (id: Identifier): Option[String] = Some(id.id)
  private val get: String => Identifier = mutableHashMapMemo(new Identifier(_))
}

object Symbol {
  def apply (s: Char): Symbol = get(s)
  def unapply (s: Symbol): Option[Char] = Some(s.symbol)
  private val get: Char => Symbol = mutableHashMapMemo(new Symbol(_))
}

object Whitespace {
  def apply (ws: Char): Whitespace = get(ws)
  private val get: Char => Whitespace = mutableHashMapMemo(new Whitespace(_))
}

object CharLiteral {
  def apply (lit: Char): CharLiteral = new CharLiteral(lit)
}

object StrLiteral {
  def apply (lit: String): StrLiteral = new StrLiteral(lit)
}

object ErrorToken {
  def apply (msg: String): ErrorToken = new ErrorToken(msg)
}
