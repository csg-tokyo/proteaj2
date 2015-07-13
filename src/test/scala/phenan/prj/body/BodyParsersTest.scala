package phenan.prj.body

import java.io.StringReader

import org.scalatest._

import phenan.prj._
import phenan.prj.ir._
import phenan.prj.state._

import scala.util._

class BodyParsersTest extends FunSuite with Matchers {
  test ("メソッドボディ") {
    val src =
      """package test;
        |public class Test0 {
        |  public static void main (String[] args) {
        |    System.out.println("Hello, world!");
        |  }
        |}
      """.stripMargin
    val file = makeIR(src)
    val test0 = file.modules.head
    val env = file.environment.staticEnvironment(test0)

    val body =
      """{
        |  System.out.println("Hello, world!");
        |}
      """.stripMargin

    val result = parsers.parse(parsers.StatementParsers(compiler.typeLoader.void, env).block, body)
    result shouldBe a [Success[_]]

    val outField = compiler.classLoader.loadClass_PE("java/lang/System").flatMap(_.classModule.findField("out", test0))

    val printMethods = compiler.classLoader.loadClass_PE("java/io/PrintStream").flatMap(_.objectType(Nil)).map(_.findMethod("println", test0, false)).getOrElse(Nil)
    val printMethod = printMethods.find { m =>
      m.erasedParameterTypes.headOption.exists { param =>
        compiler.classLoader.loadClass_PE("java/lang/String").exists { string =>
          string.isSubclassOf(param)
        }
      }
    }
    val expected = IRBlock(List(IRExpressionStatement(IRInstanceMethodCall(IRStaticFieldAccess(outField.get), Map.empty, printMethod.get, List(IRStringLiteral("Hello, world!", compiler)), Nil))))

    result.get shouldBe expected
  }

  lazy val compiler = {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/classes/"
    implicit val state = config.configure.get
    new JCompiler
  }

  lazy val parsers = new BodyParsers(compiler)

  def makeIR (src: String): IRFile = compiler.declarationCompiler.compile(new StringReader(src), "testsrc.java").get
}