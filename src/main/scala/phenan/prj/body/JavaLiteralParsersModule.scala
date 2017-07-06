package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scala.util.parsing.input.CharArrayReader.EofCh

trait JavaLiteralParsersModule {
  this: CommonParsersModule with ContextSensitiveParsersModule with JTypeLoader with IRExpressions with JModules =>
  trait JavaLiteralParsers {
    this: CommonParsers with ContextSensitiveParsers =>

    def javaLiteral(expected: JType): ContextFreeScanner[IRExpression] = JavaLiteralParsersImpl.literal(expected)

    object JavaLiteralParsersImpl {
      def literal(expected: JType): ContextFreeScanner[IRExpression] = expected match {
        case prm: JPrimitiveType => primitiveTypeLiterals.getOrElse(prm, ContextFreeScanner.failure("not implemented"))
        case _ if stringType <:< expected => stringLiteral | nullLiteral(expected)
        case _ => nullLiteral(expected)
      }

      lazy val primitiveTypeLiterals: Map[JPrimitiveType, ContextFreeScanner[IRExpression]] = Map(
        charType    -> charLiteral,
        intType     -> intLiteral,
        longType    -> longLiteral,
        booleanType -> booleanLiteral
      )

      lazy val intLiteral: ContextFreeScanner[IRIntLiteral] = integerLiteral ^? { case n if Int.MinValue <= n && n <= Int.MaxValue => IRIntLiteral(n.toInt) }

      lazy val longLiteral: ContextFreeScanner[IRLongLiteral] = integerLiteral <~ (elem('l') | elem('L')).? ^^ IRLongLiteral

      lazy val integerLiteral: ContextFreeScanner[Long] = decimalLiteral | hexLiteral | binaryLiteral | octalLiteral | zeroLiteral

      lazy val decimalLiteral: ContextFreeScanner[Long] = nonZeroDigit ~ decimalDigit.* ^^ {
        case d ~ ds => ds.foldLeft(d.toLong) {
          _ * 10 + _
        }
      }

      lazy val binaryLiteral: ContextFreeScanner[Long] = zero ~> (elem('b') | elem('B')) ~> binaryDigit.+ ^^ {
        _.foldLeft(0L) {
          _ * 2 + _
        }
      }

      lazy val octalLiteral: ContextFreeScanner[Long] = zero ~> octalDigit.+ ^^ {
        _.foldLeft(0L) {
          _ * 8 + _
        }
      }

      lazy val hexLiteral: ContextFreeScanner[Long] = zero ~> (elem('x') | elem('X')) ~> hexDigit.+ ^^ {
        _.foldLeft(0L) {
          _ * 16 + _
        }
      }

      lazy val zeroLiteral: ContextFreeScanner[Long] = zero ^^^ 0L

      lazy val charLiteral: ContextFreeScanner[IRCharLiteral] = quote ~> (escapeSequence | except('\'')) <~ quote ^^ IRCharLiteral

      lazy val booleanLiteral: ContextFreeScanner[IRBooleanLiteral] = word("true") ^^^ IRBooleanLiteral(true) | word("false") ^^^ IRBooleanLiteral(false)

      lazy val stringLiteral: ContextFreeScanner[IRStringLiteral] = dq ~> (escapeSequence | except('\"')).* <~ dq ^^ { cs => IRStringLiteral(cs.mkString) }

      def nullLiteral(expected: JType): ContextFreeScanner[IRNullLiteral] = nul ^^^ IRNullLiteral(expected)

      lazy val escapeSequence: ContextFreeScanner[Char] = octalEscape | escape('b', '\b') | escape('f', '\f') | escape('n', '\n') | escape('r', '\r') | escape('t', '\t') | escape('\'', '\'') | escape('\"', '\"') | escape('\\', '\\')

      def escape(symbol: Char, character: Char): ContextFreeScanner[Char] = backSlash ~> elem(symbol) ^^^ character

      lazy val octalEscape: ContextFreeScanner[Char] = octalEscape3 | octalEscape2 | octalEscape1

      lazy val octalEscape1: ContextFreeScanner[Char] = backSlash ~> octalDigit ^^ { _.toChar }
      lazy val octalEscape2: ContextFreeScanner[Char] = backSlash ~> (octalDigit ~ octalDigit) ^^ { case a ~ b => ((a << 3) + b).toChar }
      lazy val octalEscape3: ContextFreeScanner[Char] = backSlash ~> (quaternaryDigit ~ octalDigit ~ octalDigit) ^^ { case a ~ b ~ c => ((a << 6) + (b << 3) + c).toChar }

      lazy val decimalDigit: ContextFreeScanner[Int] = elem("decimal digit", { ch => '0' <= ch && ch <= '9' }) ^^ {
        Character.digit(_, 10)
      }
      lazy val nonZeroDigit: ContextFreeScanner[Int] = elem("non zero digit", { ch => '1' <= ch && ch <= '9' }) ^^ {
        Character.digit(_, 10)
      }
      lazy val quaternaryDigit: ContextFreeScanner[Int] = elem("quaternary digit", { ch => '0' <= ch && ch <= '3' }) ^^ {
        Character.digit(_, 4)
      }
      lazy val octalDigit: ContextFreeScanner[Int] = elem("octal digit", { ch => '0' <= ch && ch <= '7' }) ^^ {
        Character.digit(_, 8)
      }
      lazy val hexDigit: ContextFreeScanner[Int] = elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ {
        Character.digit(_, 16)
      }
      lazy val binaryDigit: ContextFreeScanner[Int] = zero ^^^ 0 | one ^^^ 1

      private lazy val zero = elem('0')
      private lazy val one = elem('1')
      private lazy val backSlash = elem('\\')
      private lazy val quote = elem('\'')
      private lazy val dq = elem('\"')
      private lazy val nul = word("null")

      def except(cs: Char*): ContextFreeScanner[Char] = elem("", c => !cs.contains(c) && c != EofCh)
    }
  }
}