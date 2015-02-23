package phenan.prj.decl

import java.io._

import org.scalatest._

import phenan.prj.state._

import scala.util.Success

class SourceReaderTest extends FunSuite with Matchers {
  implicit val state  = JConfig().configure.get

  test("識別子") {
    val src = "foo bar0 _baz$Baz"
    val in = SourceReader(source(src)).get

    in.next shouldBe IdentifierToken("foo", 1)
    in.next shouldBe IdentifierToken("bar0", 1)
    in.next shouldBe IdentifierToken("_baz$Baz", 1)
    in.next shouldBe EndOfSource
  }

  test("文字リテラル") {
    val src = "'a''b''\\n'"
    val in = SourceReader(source(src)).get

    in.next shouldBe CharLitToken('a', 1)
    in.next shouldBe CharLitToken('b', 1)
    in.next shouldBe CharLitToken('\n', 1)
    in.next shouldBe EndOfSource
  }

  test("文字列リテラル") {
    val src = "\"ab cd\" \"'x'\" \"\\\"\\\"\""
    val in = SourceReader(source(src)).get

    in.next shouldBe StringLitToken("ab cd", 1)
    in.next shouldBe StringLitToken("'x'", 1)
    in.next shouldBe StringLitToken("\"\"", 1)
    in.next shouldBe EndOfSource
  }

  test("記号") {
    val src = "* +32=> [/"
    val in = SourceReader(source(src)).get

    in.next shouldBe SymbolToken('*', 1)
    in.next shouldBe SymbolToken('+', 1)
    in.next shouldBe SymbolToken('3', 1)
    in.next shouldBe SymbolToken('2', 1)
    in.next shouldBe SymbolToken('=', 1)
    in.next shouldBe SymbolToken('>', 1)
    in.next shouldBe SymbolToken('[', 1)
    in.next shouldBe SymbolToken('/', 1)
    in.next shouldBe EndOfSource
  }

  test("行コメント") {
    val src =
      """
        |// x y x
        |// foo bar baz
      """.stripMargin
    val in = SourceReader(source(src)).get

    in.next shouldBe EndOfSource
  }

  test("ブロックコメント") {
    val src =
      """
        |/* ""
        | * 'a'
        | **/
      """.stripMargin
    val in = SourceReader(source(src)).get

    in.next shouldBe EndOfSource
  }

  test("先読み") {
    val src = """ int func = '\n'; """
    val in = SourceReader(source(src)).get

    in.look(0) shouldBe IdentifierToken("int", 1)
    in.look(1) shouldBe IdentifierToken("func", 1)
    in.look(2) shouldBe SymbolToken('=', 1)
    in.look(3) shouldBe CharLitToken('\n', 1)
    in.look(4) shouldBe SymbolToken(';', 1)
    in.look(5) shouldBe EndOfSource

    in.next shouldBe IdentifierToken("int", 1)
    in.next shouldBe IdentifierToken("func", 1)
    in.next shouldBe SymbolToken('=', 1)
    in.next shouldBe CharLitToken('\n', 1)
    in.next shouldBe SymbolToken(';', 1)
    in.next shouldBe EndOfSource
  }

  test("フィールド初期化部の取り出し") {
    val src = """ int func = '\n'; """
    val in = SourceReader(source(src)).get

    in.next shouldBe IdentifierToken("int", 1)
    in.next shouldBe IdentifierToken("func", 1)
    in.next shouldBe SymbolToken('=', 1)
    in.nextExpression shouldBe Success(Snippet(" '\\n'", 1))
    in.next shouldBe SymbolToken(';', 1)
    in.next shouldBe EndOfSource
  }

  test("フィールド初期化部の取り出し(先読み済み)") {
    val src = """ int func = '\n'; """
    val in = SourceReader(source(src)).get

    in.look(0) shouldBe IdentifierToken("int", 1)
    in.look(1) shouldBe IdentifierToken("func", 1)
    in.look(2) shouldBe SymbolToken('=', 1)
    in.look(3) shouldBe CharLitToken('\n', 1)
    in.look(4) shouldBe SymbolToken(';', 1)
    in.look(5) shouldBe EndOfSource

    in.next shouldBe IdentifierToken("int", 1)
    in.next shouldBe IdentifierToken("func", 1)
    in.next shouldBe SymbolToken('=', 1)
    in.nextExpression shouldBe Success(Snippet(" '\\n'", 1))
    in.next shouldBe SymbolToken(';', 1)
    in.next shouldBe EndOfSource
  }

  test("フルアノテーションの実引数部の取り出し") {
    val src = """@Annotation (a = (1 + 2) * 3, b = Foo.class )"""
    val in = SourceReader(source(src)).get

    in.next shouldBe SymbolToken('@', 1)
    in.next shouldBe IdentifierToken("Annotation", 1)
    in.next shouldBe SymbolToken('(', 1)
    in.next shouldBe IdentifierToken("a", 1)
    in.next shouldBe SymbolToken('=', 1)
    in.nextExpression shouldBe Success(Snippet(" (1 + 2) * 3", 1))
    in.next shouldBe SymbolToken(',', 1)
    in.next shouldBe IdentifierToken("b", 1)
    in.next shouldBe SymbolToken('=', 1)
    in.nextExpression shouldBe Success(Snippet(" Foo.class ", 1))
    in.next shouldBe SymbolToken(')', 1)
    in.next shouldBe EndOfSource
  }

  test("単純なブロックの取得") {
    val src = """ { a b c d } """
    val in = SourceReader(source(src)).get

    in.next shouldBe SymbolToken('{', 1)
    in.nextBlock shouldBe Success(Snippet(" a b c d ", 1))
    in.next shouldBe SymbolToken('}', 1)
    in.next shouldBe EndOfSource
  }

  test("中括弧を含むブロックの取得") {
    val src =
      """int fib (int n) {
        |  if (n < 2) return 1;
        |  else {
        |    int a = fib(n - 1);
        |    int b = fib(n - 2);
        |    return a + b;
        |  }
        |} """.stripMargin
    val in = SourceReader(source(src)).get

    val body =
      """
        |  if (n < 2) return 1;
        |  else {
        |    int a = fib(n - 1);
        |    int b = fib(n - 2);
        |    return a + b;
        |  }
        |""".stripMargin

    in.next shouldBe IdentifierToken("int", 1)
    in.next shouldBe IdentifierToken("fib", 1)
    in.next shouldBe SymbolToken('(', 1)
    in.next shouldBe IdentifierToken("int", 1)
    in.next shouldBe IdentifierToken("n", 1)
    in.next shouldBe SymbolToken(')', 1)
    in.next shouldBe SymbolToken('{', 1)
    in.nextBlock shouldBe Success(Snippet(body, 1))
    in.next shouldBe SymbolToken('}', 8)
    in.next shouldBe EndOfSource
  }

  test("正常終了したか") {
    state.clean()
    state.errors shouldBe 0
    state.warns shouldBe 0
  }

  def source (src: String): Reader = new StringReader(src)
}
