package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scala.util.parsing.input.CharArrayReader.EofCh

trait JavaLiteralParsers {
  this: CommonParsers =>

  def compiler: JCompiler

  def javaLiteral (expected: JType): LParser[IRExpression] = JavaLiteralParsersImpl.literal(expected)

  object JavaLiteralParsersImpl {
    def literal (expected: JType): LParser[IRExpression] = expected match {
      case prm: JPrimitiveType => primitiveTypeLiterals.getOrElse(prm, LParser.failure("not implemented"))
      case _ if compiler.typeLoader.stringType.exists(_ <:< expected) => stringLiteral | nullLiteral(expected)
      case _ => nullLiteral(expected)
    }

    lazy val primitiveTypeLiterals: Map[JPrimitiveType, LParser[IRExpression]] = Map (
      compiler.typeLoader.char    -> charLiteral,
      compiler.typeLoader.int     -> intLiteral,
      compiler.typeLoader.long    -> longLiteral,
      compiler.typeLoader.boolean -> booleanLiteral
    )

    lazy val intLiteral: LParser[IRIntLiteral] = integerLiteral ^? { case n if Int.MinValue <= n && n <= Int.MaxValue => IRIntLiteral(n.toInt, compiler) }

    lazy val longLiteral: LParser[IRLongLiteral] = integerLiteral <~ ( elem('l') | elem('L') ) ^^ { value => IRLongLiteral(value, compiler) }

    lazy val integerLiteral: LParser[Long] = decimalLiteral | hexLiteral | binaryLiteral | octalLiteral | zeroLiteral

    lazy val decimalLiteral: LParser[Long] = nonZeroDigit ~ decimalDigit.* ^^ {
      case d ~ ds => ds.foldLeft(d.toLong) { _ * 10 + _ }
    }

    lazy val binaryLiteral: LParser[Long] = zero ~> ( elem('b') | elem('B') ) ~> binaryDigit.+ ^^ { _.foldLeft(0L) { _ * 2 + _ } }

    lazy val octalLiteral: LParser[Long] = zero ~> octalDigit.+ ^^ { _.foldLeft(0L) { _ * 8 + _ } }

    lazy val hexLiteral: LParser[Long] = zero ~> ( elem('x') | elem('X') ) ~> hexDigit.+ ^^ { _.foldLeft(0L) { _ * 16 + _ } }

    lazy val zeroLiteral: LParser[Long] = zero ^^^ 0L

    lazy val charLiteral: LParser[IRCharLiteral] = quote ~> (escapeSequence | except('\'')) <~ quote ^^ { value => IRCharLiteral(value, compiler) }

    lazy val booleanLiteral: LParser[IRBooleanLiteral] = word("true") ^^^ IRBooleanLiteral(true, compiler) | word("false") ^^^ IRBooleanLiteral(false, compiler)

    lazy val stringLiteral: LParser[IRStringLiteral] = dq ~> (escapeSequence | except('\"')).* <~ dq ^^ { cs => IRStringLiteral(cs.mkString, compiler) }

    def nullLiteral (expected: JType): LParser[IRNullLiteral] = nul ^^^ IRNullLiteral(expected)

    lazy val escapeSequence = octalEscape | escape('b', '\b') | escape('f', '\f') | escape('n', '\n') | escape('r', '\r') | escape('t', '\t') | escape('\'', '\'') | escape('\"', '\"') | escape('\\', '\\')

    def escape (symbol: Char, character: Char) = backSlash ~> elem(symbol) ^^^ character

    lazy val octalEscape = octalEscape3 | octalEscape2 | octalEscape1

    lazy val octalEscape1 = backSlash ~> octalDigit ^^ { _.toChar }
    lazy val octalEscape2 = backSlash ~> ( octalDigit ~ octalDigit ) ^^ { case a ~ b => ((a << 3) + b).toChar }
    lazy val octalEscape3 = backSlash ~> ( quaternaryDigit ~ octalDigit ~ octalDigit ) ^^ { case a ~ b ~ c => ((a << 6) + (b << 3) + c).toChar }

    lazy val decimalDigit = elem("decimal digit", { ch => '0' <= ch && ch <= '9' }) ^^ { Character.digit(_, 10) }
    lazy val nonZeroDigit = elem("non zero digit", { ch => '1' <= ch && ch <= '9' }) ^^ { Character.digit(_, 10) }
    lazy val quaternaryDigit = elem("quaternary digit", { ch => '0' <= ch && ch <= '3' }) ^^ { Character.digit(_, 4) }
    lazy val octalDigit = elem("octal digit", { ch => '0' <= ch && ch <= '7' }) ^^ { Character.digit(_, 8) }
    lazy val hexDigit = elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ { Character.digit(_, 16) }
    lazy val binaryDigit  = zero ^^^ 0 | one ^^^ 1

    private lazy val zero = elem('0')
    private lazy val one = elem('1')
    private lazy val backSlash = elem('\\')
    private lazy val quote = elem('\'')
    private lazy val dq = elem('\"')
    private lazy val nul = word("null")

    def except(cs: Char*) = elem("", c => ! cs.contains(c) && c != EofCh)
  }
}
