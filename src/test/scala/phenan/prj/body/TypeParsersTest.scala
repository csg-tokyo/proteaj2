package phenan.prj.body

import java.io.StringReader

import org.scalatest._
import phenan.prj.JCompiler
import phenan.prj.state.Config

import scala.util.Success
import scala.util.parsing.input.CharSequenceReader

class TypeParsersTest extends FunSuite with Matchers {
  val compiler: JCompiler.JCompilerImpl = JCompiler.init(Config()).right.get

  import compiler._

  test ("className") {
    val program =
      """import java.io.Reader;
        |import java.util.*;
      """.stripMargin

    val file = compileDeclaration(new StringReader(program), "testsrc.java")

    file shouldBe a [Success[_]]

    val parsers = new BodyParsers(new BaseEnvironment {
      override def resolver: compiler.NameResolver = file.get.resolver
      override def declaringModule: compiler.IRModule = ???
      override def thisType: Option[compiler.JObjectType] = ???
      override def fileEnvironment: compiler.FileEnvironment = ???
    }).typeParsers

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

    val parsers = new BodyParsers(new BaseEnvironment {
      override def resolver: compiler.NameResolver = file.get.resolver
      override def declaringModule: compiler.IRModule = ???
      override def thisType: Option[compiler.JObjectType] = ???
      override def fileEnvironment: compiler.FileEnvironment = ???
    }).typeParsers

    val r1 = parsers.typeName(src("JarFile"))
    val ans1 = getObjectType(loadClass("java/util/jar/JarFile").get, Nil).get
    r1.successful shouldBe true
    r1.get shouldBe ans1

    val r2 = parsers.typeName(src("Map<String, JarEntry>"))
    val str = getObjectType(loadClass("java/lang/String").get, Nil).get
    val je = getObjectType(loadClass("java/util/jar/JarEntry").get, Nil).get
    val ans2 = getObjectType(loadClass("java/util/Map").get, List(str, je)).get
    r2.successful shouldBe true
    r2.get shouldBe ans2

    val r3 = parsers.typeName(src("Map.Entry<String, java.util.jar.JarEntry>"))
    val ans3 = getObjectType(loadClass(s"java/util/Map$$Entry").get, List(str, je)).get
    r3.successful shouldBe true
    r3.get shouldBe ans3
  }

  def src (s: String) = new CharSequenceReader(s)
}
