package phenan.prj.body

import java.io.StringReader

import org.scalatest._

import phenan.prj._
import phenan.prj.ir._
import phenan.prj.state._

import scala.util._

class BodyParsersTest extends FunSuite with Matchers {
  test ("this") {
    val src =
      """package test;
        |public class Test0 {
        |  public Test0 returnThis () {
        |    return this;
        |  }
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val expected = IRMethodBody(IRBlock(List(IRReturnStatement(IRThisRef(test0.thisType.get)))))

    body shouldBe Some(expected)
  }

  test ("this field access") {
    val src =
      """package test;
        |public class Test0 {
        |  public int returnField () {
        |    return this.n;
        |  }
        |  private int n;
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val thisType = test0.thisType.get
    val expected = IRMethodBody(IRBlock(List(IRReturnStatement(IRInstanceFieldAccess(IRThisRef(thisType), thisType.findField("n", test0, true).get)))))

    body shouldBe Some(expected)
  }

  test ("ローカル変数アクセス") {
    val src =
      """package test;
        |public class Test0 {
        |  public int id (int n) {
        |    return n;
        |  }
        |  private int n;
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val expected = IRMethodBody(IRBlock(List(IRReturnStatement(IRLocalVariableRef(compiler.typeLoader.int, "n")))))

    body shouldBe Some(expected)
  }

  test ("代入") {
    val src =
      """package test;
        |public class Test0 {
        |  public void setField (int n) {
        |    this.n = n;
        |  }
        |  private int n;
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val thisType = test0.thisType.get
    val expected = IRMethodBody(IRBlock(List(IRExpressionStatement(
      IRSimpleAssignmentExpression(IRInstanceFieldAccess(IRThisRef(thisType), thisType.findField("n", test0, true).get),
        IRLocalVariableRef(compiler.typeLoader.int, "n"))))))

    body shouldBe Some(expected)
  }

  test ("静的関数呼び出し") {
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
    val mainMethod = test0.procedures.head

    val body =
      """{
        |  System.out.println("Hello, world!");
        |}
      """.stripMargin

    val result = parsers.parse(parsers.getStatementParsers(compiler.typeLoader.void, mainMethod.environment).block, body)
    result shouldBe a [Success[_]]

    val outField = compiler.classLoader.loadClass_PE("java/lang/System").flatMap(_.classModule.findField("out", test0))

    val printMethods = compiler.classLoader.loadClass_PE("java/io/PrintStream").flatMap(_.objectType(Nil)).map(_.findMethod("println", test0, false)).getOrElse(Nil)
    val printMethod = printMethods.find { m =>
      m.erasedParameterTypes.headOption.exists { param =>
        compiler.classLoader.loadClass_PE("java/lang/String").contains(param)
      }
    }
    val expected = IRBlock(List(IRExpressionStatement(IRInstanceMethodCall(IRStaticFieldAccess(outField.get), Map.empty, printMethod.get, List(IRStringLiteral("Hello, world!", compiler)), Nil))))

    result.get shouldBe expected

    mainMethod.asInstanceOf[IRMethod].methodBody shouldBe Some(IRMethodBody(expected))
  }

  test ("配列アクセス") {
    val src =
      """package test;
        |public class Test0 {
        |  public static void main (String[] args) {
        |    System.out.println(args[0]);
        |  }
        |}
      """.stripMargin
    val file = makeIR(src)
    val test0 = file.modules.head
    val mainMethod = test0.procedures.head

    val outField = compiler.classLoader.loadClass_PE("java/lang/System").flatMap(_.classModule.findField("out", test0))

    val printMethods = compiler.classLoader.loadClass_PE("java/io/PrintStream").flatMap(_.objectType(Nil)).map(_.findMethod("println", test0, false)).getOrElse(Nil)
    val printMethod = printMethods.find { m =>
      m.erasedParameterTypes.headOption.exists { param =>
        compiler.classLoader.loadClass_PE("java/lang/String").contains(param)
      }
    }

    val result = mainMethod.asInstanceOf[IRMethod].methodBody

    val arrayOfString = compiler.typeLoader.stringType.map(_.array).get

    val expected = IRMethodBody(IRBlock(List(IRExpressionStatement(IRInstanceMethodCall(IRStaticFieldAccess(outField.get), Map.empty, printMethod.get, List(IRArrayAccess(IRLocalVariableRef(arrayOfString, "args"), IRIntLiteral(0, compiler))), Nil)))))

    result shouldBe Some(expected)
  }

  test ("ジェネリクス・ローカル変数アクセス") {
    val src =
      """package test;
        |public class Test0 <T> {
        |  public T id (T n) {
        |    return n;
        |  }
        |  private T n;
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val expected = IRMethodBody(IRBlock(List(IRReturnStatement(IRLocalVariableRef(JTypeVariable("T", Nil, compiler), "n")))))

    body shouldBe Some(expected)
  }

  test ("ジェネリクス・代入") {
    val src =
      """package test;
        |public class Test0 <T> {
        |  public void setField (T n) {
        |    this.n = n;
        |  }
        |  private T n;
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val thisType = test0.thisType.get
    val expected = IRMethodBody(IRBlock(List(IRExpressionStatement(
      IRSimpleAssignmentExpression(IRInstanceFieldAccess(IRThisRef(thisType), thisType.findField("n", test0, true).get),
        IRLocalVariableRef(JTypeVariable("T", Nil, compiler), "n"))))))

    body shouldBe Some(expected)
  }

  lazy val compiler = {
    val config = JConfig()
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/classes/"
    new JCompiler(config.configure.get)
  }

  lazy val parsers = new BodyParsers(compiler)

  def makeIR (src: String): IRFile = {
    val file = compiler.declarationCompiler.compile(new StringReader(src), "testsrc.java").get
    compiler.registerIR(file)
    file
  }
}
