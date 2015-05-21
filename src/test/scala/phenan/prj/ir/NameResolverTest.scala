package phenan.prj.ir

import java.io.StringReader

import org.scalatest._

import phenan.prj.JCompiler
import phenan.prj.state.JConfig

import scala.util._

class NameResolverTest extends FunSuite with Matchers {
  test ("完全修飾名によるロード") {
    implicit val state = JConfig().configure.get
    val compiler = new JCompiler
    val resolver: NameResolver = NameResolver.root(compiler)

    resolver.resolve(List("java", "util", "ArrayList")) shouldBe a [Success[_]]
    resolver.resolve(List("java", "util", "ArrayList")).get shouldBe compiler.classLoader.load("java/util/ArrayList").get

    resolver.resolve(List("java", "util", "Map", "Entry")) shouldBe a [Success[_]]
    resolver.resolve(List("java", "util", "Map", "Entry")).get shouldBe compiler.classLoader.load("java/util/Map$Entry").get
  }

  test ("短縮名称によるロード") {
    implicit val state = JConfig().configure.get
    val compiler = new JCompiler
    val resolver: NameResolver = NameResolver.root(compiler)

    resolver.resolve(List("System")) shouldBe a [Success[_]]
    resolver.resolve(List("System")).get shouldBe compiler.classLoader.load("java/lang/System").get
  }

  test ("パッケージ、インポート") {
    val program =
      """package java.awt;
        |import java.io.File;
        |import java.util.*;
      """.stripMargin
    implicit val state = JConfig().configure.get
    val compiler = new JCompiler
    val file = compiler.declarationCompiler.compile(new StringReader(program), "testsrc.java")

    file shouldBe a [Success[_]]
    val resolver = file.get.resolver

    resolver.resolve(List("Color")) shouldBe a [Success[_]]
    resolver.resolve(List("Color")).get shouldBe compiler.classLoader.load("java/awt/Color").get

    resolver.resolve(List("List")) shouldBe a [Success[_]]
    resolver.resolve(List("List")).get shouldBe compiler.classLoader.load("java/awt/List").get

    resolver.resolve(List("File")) shouldBe a [Success[_]]
    resolver.resolve(List("File")).get shouldBe compiler.classLoader.load("java/io/File").get

    resolver.resolve(List("Reader")) shouldBe a [Failure[_]]

    resolver.resolve(List("ArrayList")) shouldBe a [Success[_]]
    resolver.resolve(List("ArrayList")).get shouldBe compiler.classLoader.load("java/util/ArrayList").get

    resolver.resolve(List("Map", "Entry")) shouldBe a [Success[_]]
    resolver.resolve(List("Map", "Entry")).get shouldBe compiler.classLoader.load("java/util/Map$Entry").get
  }
}
