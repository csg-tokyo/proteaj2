package phenan.prj.ir

import java.io.StringReader

import org.scalatest._

import phenan.prj._
import phenan.prj.ir._
import phenan.prj.state._

class EnvironmentTest extends FunSuite with Matchers {
  test ("ジェネリクス") {
    val src =
      """package test;
        |public class Test0 <T> {
        |  public void setField (T n) {
        |    this.n = n;
        |  }
        |  private T n;
        |}
      """.stripMargin



    val module = compile(src, "test/Test0").get
    val method = module.procedures.head
    method.environment.localVariable("n") shouldBe a [Some[_]]

    method.environment.resolver.typeVariable("T") shouldBe a [Some[_]]
  }

  lazy val compiler = {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/classes/"
    implicit val state = config.configure.get
    new JCompiler
  }

  def compile (src: String, name: String) = {
    compiler.generateIR(new StringReader(src), name + ".java")
    compiler.findIR(name)
  }
}
