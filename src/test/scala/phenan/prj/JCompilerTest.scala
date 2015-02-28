package phenan.prj

import org.scalatest._
import phenan.prj.ir._
import phenan.prj.state._

class JCompilerTest extends FunSuite with Matchers {
  val config = new JConfig
  config.sourcePath = "/Users/ichikawa/workspaces/Idea/prj/src/test/java"
  implicit val state = config.configure.get

  test ("Hello をコンパイルしてみる") {
    val compiler = new JCompiler()

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Hello.java"))
    val clazz = compiler.findIR("test/Hello")
    clazz shouldBe a [Some[_]]
    clazz.get shouldBe a [IRClass]

    val ir = clazz.get.asInstanceOf[IRClass]
    ir.methods should have (size (2))
  }

  test ("Foo をコンパイルしてみる") {
    val compiler = new JCompiler()

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Foo.java"))
    val foo = compiler.findIR("test/Foo")
    foo shouldBe a [Some[_]]
    foo.get shouldBe a [IRClass]

    val fir = foo.get.asInstanceOf[IRClass]
    fir.fields should have (size (1))

    val field = fir.fields.head

    val hello = compiler.findIR("test/Hello")
    hello shouldBe a [Some[_]]
    hello.get shouldBe a [IRClass]

    val hir = hello.get.asInstanceOf[IRClass]
    hir.methods should have (size (2))

    field.fieldType shouldBe hir
  }
}
