package phenan.prj.body

import java.io.StringReader

import org.scalatest._
import phenan.prj.JCompiler
import phenan.prj.state.JConfig

import scala.util.Success
import scala.util.parsing.input.CharSequenceReader

class TypeParsersTest extends FunSuite with Matchers {
  test ("className") {
    val program =
      """import java.io.Reader;
        |import java.util.*;
      """.stripMargin
    val compiler = new JCompiler(JConfig().configure.get)
    val file = compiler.declarationCompiler.compile(new StringReader(program), "testsrc.java")

    file shouldBe a [Success[_]]
    val resolver = file.get.resolver

    val bp = new BodyParsers(compiler)
    val parsers = bp.getTypeParsers(resolver)

    val r1 = parsers.className(src("Reader"))
    r1.successful shouldBe true
    r1.get shouldBe compiler.classLoader.loadClass("java/io/Reader").get

    val r2 = parsers.className(src("Set"))
    r2.successful shouldBe true
    r2.get shouldBe compiler.classLoader.loadClass("java/util/Set").get

    val r3 = parsers.className(src("java.lang.System"))
    r3.successful shouldBe true
    r3.get shouldBe compiler.classLoader.loadClass("java/lang/System").get

    val r4 = parsers.className(src("java.util.Map.Entry"))
    r4.successful shouldBe true
    r4.get shouldBe compiler.classLoader.loadClass("java/util/Map$Entry").get
  }

  test("typeName") {
    val program =
      """import java.util.jar.*;
        |import java.util.Map;
      """.stripMargin
    val compiler = new JCompiler(JConfig().configure.get)
    val file = compiler.declarationCompiler.compile(new StringReader(program), "testsrc.java")

    file shouldBe a [Success[_]]
    val resolver = file.get.resolver

    val bp = new BodyParsers(compiler)
    val parsers = bp.getTypeParsers(resolver)

    val r1 = parsers.typeName(src("JarFile"))
    val ans1 = compiler.classLoader.loadClass("java/util/jar/JarFile").get.objectType(Nil).get
    r1.successful shouldBe true
    r1.get shouldBe ans1

    val r2 = parsers.typeName(src("Map<String, JarEntry>"))
    val str = compiler.classLoader.loadClass("java/lang/String").get.objectType(Nil).get
    val je = compiler.classLoader.loadClass("java/util/jar/JarEntry").get.objectType(Nil).get
    val ans2 = compiler.classLoader.loadClass("java/util/Map").get.objectType(List(str, je)).get
    r2.successful shouldBe true
    r2.get shouldBe ans2

    val r3 = parsers.typeName(src("Map.Entry<String, java.util.jar.JarEntry>"))
    val ans3 = compiler.classLoader.loadClass("java/util/Map$Entry").get.objectType(List(str, je)).get
    r3.successful shouldBe true
    r3.get shouldBe ans3
  }

  def src (s: String) = new CharSequenceReader(s)
}
