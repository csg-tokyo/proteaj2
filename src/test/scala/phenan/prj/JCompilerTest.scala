package phenan.prj

import org.scalatest._
import scala.sys.process._

class JCompilerTest extends FunSuite with Matchers with BeforeAndAfterAll {
  test ("PrintDSL") {
    val result = compileAndRun("print.Main", List("src/test/proteaj/print/PrintDSL.pj", "src/test/proteaj/print/Main.pj"))
    result shouldBe "Hello, world!\n"
  }

  test ("LetDSL") {
    val result = compileAndRun("let.Main", List("src/test/proteaj/let/LetDSL.pj", "src/test/proteaj/let/Main.pj"))
    result shouldBe "hello\n"
  }

  test ("FileDSL") {
    val result = compileAndRun("file.Main", List("src/test/proteaj/file/FileDSL.pj", "src/test/proteaj/file/Main.pj"))
    result shouldBe "The ProteaJ compiler\n"
  }

  test ("LetDSL2") {
    val result = compileAndRun("let2.Main", List("src/test/proteaj/let2/LetDSL.pj", "src/test/proteaj/let2/Main.pj"))
    result shouldBe "hello\n"
  }

  test ("IdentifierDSL") {
    val result = compileAndRun("regname.Main", List("src/test/proteaj/regname/IdentifierDSL.pj", "src/test/proteaj/regname/Main.pj"))
    result shouldBe "hello\n"
  }

  test ("LetDSL3") {
    val result = compileAndRun("let3.Main", List("src/test/proteaj/let3/LetDSL.pj", "src/test/proteaj/let3/Main.pj"))
    result shouldBe "hello\n"
  }

  test ("LetDSL4") {
    val result = compileAndRun("let4.Main", List("src/test/proteaj/let4/LetDSL.pj", "src/test/proteaj/let4/Main.pj"))
    result shouldBe "hello\n"
  }

  test ("ForDSL1") {
    val result = compileAndRun("for1.Main", List("src/test/proteaj/for1/ForDSL.pj", "src/test/proteaj/for1/Main.pj"))
    result shouldBe "HELLO\nWORLD\n"
  }

  test ("StructDSL") {
    val result = compileAndRun("struct.Main", List("src/test/proteaj/struct/StructDSL.pj", "src/test/proteaj/struct/Main.pj"))
    result shouldBe "hello\nworld\ngoodbye\nworld\n"
  }

  test ("MatchDSL") {
    val result = compileAndRun("match.Main", List("src/test/proteaj/match/MatchDSL.pj", "src/test/proteaj/match/Main.pj"))
    result shouldBe "b\n"
  }

  override protected def beforeAll(): Unit = {
    if (new java.io.File("bin").isDirectory) "rm -r bin" ! ProcessLogger(_ => ())
    "mkdir bin" ! ProcessLogger(_ => ())
    ()
  }

  def compileAndRun (mainClass: String, files: List[String]): String = {
    JCompiler.main((List("-d", "bin") ++ files).toArray)
    ( "java -cp bin:target/scala-2.12/classes " + mainClass ).!!
  }
}
