package phenan.prj.ir

import java.io.StringReader

import org.scalatest._

import phenan.prj._
import phenan.prj.state._

class EnvironmentTest extends FunSuite with Matchers {
  val compiler: JCompiler.JCompilerImpl = JCompiler.init(Config()).right.get

  import compiler._

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

  def compile (src: String, name: String): Option[IRModule] = {
    compiler.compileFile(new StringReader(src), name + ".java")
    compiler.findIR(name)
  }
}
