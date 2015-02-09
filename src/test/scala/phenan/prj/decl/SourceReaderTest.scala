package phenan.prj.decl

import java.io._

import org.scalatest._

import phenan.prj.state._

class SourceReaderTest extends FunSuite with Matchers {
  implicit val state  = JConfig().configure.get

  test("識別子") {
    val src = "foo bar0 _baz$Baz"
    val in = SourceReader(source(src)).get

    in.nextToken shouldBe IdentifierToken("foo")
    in.nextToken shouldBe IdentifierToken("bar0")
    in.nextToken shouldBe IdentifierToken("_baz$Baz")
    in.nextToken shouldBe EndOfSource
  }

  test("文字リテラル") {
    val src = "'a''b''\\n'"
    val in = SourceReader(source(src)).get

    in.nextToken shouldBe CharLitToken("a")
    in.nextToken shouldBe CharLitToken("b")
    in.nextToken shouldBe CharLitToken("\\n")
    in.nextToken shouldBe EndOfSource
  }

  test("文字列リテラル") {
    val src = "\"ab cd\" \"'x'\" \"\\\"\\\"\""
    val in = SourceReader(source(src)).get

    in.nextToken shouldBe StringLitToken("ab cd")
    in.nextToken shouldBe StringLitToken("'x'")
    in.nextToken shouldBe StringLitToken("\\\"\\\"")
    in.nextToken shouldBe EndOfSource
  }

  test("記号") {
    val src = "* +32=> [/"
    val in = SourceReader(source(src)).get

    in.nextToken shouldBe SymbolToken('*')
    in.nextToken shouldBe SymbolToken('+')
    in.nextToken shouldBe SymbolToken('3')
    in.nextToken shouldBe SymbolToken('2')
    in.nextToken shouldBe SymbolToken('=')
    in.nextToken shouldBe SymbolToken('>')
    in.nextToken shouldBe SymbolToken('[')
    in.nextToken shouldBe SymbolToken('/')
    in.nextToken shouldBe EndOfSource
  }


  def source (src: String): Reader = new StringReader(src)
}
