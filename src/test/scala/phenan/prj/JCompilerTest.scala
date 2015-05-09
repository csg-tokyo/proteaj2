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

    clazz.get.methods should have (size (2))
  }

  test ("Foo をコンパイルしてみる") {
    val compiler = new JCompiler()

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Hello.java", "/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Foo.java"))
    compiler.findIR("test/Hello") shouldBe a [Some[_]]

    val foo = compiler.findIR("test/Foo")
    foo shouldBe a [Some[_]]
    foo.get shouldBe a [IRClass]

    foo.get.fields should have (size (1))

    val field = foo.get.fields.head

    val hello = compiler.findIR("test/Hello")
    hello shouldBe a [Some[_]]
    hello.get shouldBe a [IRClass]

    hello.get.methods should have (size (2))
  }
}
