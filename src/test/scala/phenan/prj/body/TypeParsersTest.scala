package phenan.prj.body

import java.io.StringReader

import org.scalatest._
import phenan.prj.JCompiler
import phenan.prj.state.Config

import scala.util.Success
import scala.util.parsing.input.CharSequenceReader

class TypeParsersTest extends FunSuite with Matchers {
  val compiler = JCompiler(Config())

  import compiler._

  private val bp = BodyParsers

  test ("className") {
    val program =
      """import java.io.Reader;
        |import java.util.*;
      """.stripMargin

    val file = compileDeclaration(new StringReader(program), "testsrc.java")

    file shouldBe a [Success[_]]
    val resolver = file.get.resolver

    val parsers = bp.getTypeParsers(resolver)

    val r1 = parsers.className(src("Reader"))
    r1.successful shouldBe true
    r1.get shouldBe loadClass("java/io/Reader").get

    val r2 = parsers.className(src("Set"))
    r2.successful shouldBe true
    r2.get shouldBe loadClass("java/util/Set").get

    val r3 = parsers.className(src("java.lang.System"))
    r3.successful shouldBe true
    r3.get shouldBe loadClass("java/lang/System").get

    val r4 = parsers.className(src("java.util.Map.Entry"))
    r4.successful shouldBe true
    r4.get shouldBe loadClass(s"java/util/Map$$Entry").get
  }

  test("typeName") {
    val program =
      """import java.util.jar.*;
        |import java.util.Map;
      """.stripMargin

    val file = compileDeclaration(new StringReader(program), "testsrc.java")

    file shouldBe a [Success[_]]
    val resolver = file.get.resolver

    val parsers = bp.getTypeParsers(resolver)

    val r1 = parsers.typeName(src("JarFile"))
    val ans1 = loadClass("java/util/jar/JarFile").get.objectType(Nil).get
    r1.successful shouldBe true
    r1.get shouldBe ans1

    val r2 = parsers.typeName(src("Map<String, JarEntry>"))
    val str = loadClass("java/lang/String").get.objectType(Nil).get
    val je = loadClass("java/util/jar/JarEntry").get.objectType(Nil).get
    val ans2 = loadClass("java/util/Map").get.objectType(List(str, je)).get
    r2.successful shouldBe true
    r2.get shouldBe ans2

    val r3 = parsers.typeName(src("Map.Entry<String, java.util.jar.JarEntry>"))
    val ans3 = loadClass(s"java/util/Map$$Entry").get.objectType(List(str, je)).get
    r3.successful shouldBe true
    r3.get shouldBe ans3
  }

  def src (s: String) = new CharSequenceReader(s)
}
