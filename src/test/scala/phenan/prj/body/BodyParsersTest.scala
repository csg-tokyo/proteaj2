package phenan.prj.body

import java.io.StringReader

import org.scalatest._

import phenan.prj._
import phenan.prj.state._

import scala.util._

class BodyParsersTest extends FunSuite with Matchers {
  val compiler: JCompiler.JCompilerImpl = JCompiler.init(Config()).right.get

  import compiler._

  def makeIR (src: String): IRFile = {
    val file = compileDeclaration(new StringReader(src), "testsrc.java").get
    registerIR(file)
    file
  }

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

    val expected = IRMethodBody(IRBlock(List(IRReturnStatement(IRLocalVariableRef(intType, "n")))))

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
        IRLocalVariableRef(intType, "n"))))))

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

    val parsers = new BodyParsers(mainMethod.environment)
    val result = parsers.getStatementParsers(voidType).block(body, mainMethod.environment)
    result shouldBe a [Success[_]]

    val outField = loadClass("java/lang/System").toOption.flatMap(_.classModule.findField("out", test0))

    val printMethods = loadClass("java/io/PrintStream").toOption.flatMap(getObjectType(_, Nil)).map(_.findMethod("println", test0, false)).getOrElse(Nil)
    val printMethod = printMethods.find { m =>
      m.erasedParameterTypes.headOption.contains(stringClass)
    }
    val expected = IRBlock(List(IRExpressionStatement(IRInstanceMethodCall(IRStaticFieldAccess(outField.get), Map.empty, printMethod.get, List(IRStringLiteral("Hello, world!")), Nil))))

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

    val outField = loadClass("java/lang/System").toOption.flatMap(_.classModule.findField("out", test0))

    val printMethods = loadClass("java/io/PrintStream").toOption.flatMap(getObjectType(_, Nil)).map(_.findMethod("println", test0, false)).getOrElse(Nil)
    val printMethod = printMethods.find { m =>
      m.erasedParameterTypes.headOption.contains(stringClass)
    }

    val result = mainMethod.asInstanceOf[IRMethod].methodBody

    val arrayOfString = stringType.map(_.array).get

    val expected = IRMethodBody(IRBlock(List(IRExpressionStatement(IRInstanceMethodCall(IRStaticFieldAccess(outField.get), Map.empty, printMethod.get, List(IRArrayAccess(IRLocalVariableRef(arrayOfString, "args"), IRIntLiteral(0))), Nil)))))

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

    val expected = IRMethodBody(IRBlock(List(IRReturnStatement(IRLocalVariableRef(JTypeVariable("T", Nil), "n")))))

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
        IRLocalVariableRef(JTypeVariable("T", Nil), "n"))))))

    body shouldBe Some(expected)
  }

  test ("変数宣言") {
    val src =
      """package test;
        |public class Test0 <T> {
        |  public void test () {
        |    String s = "hello, world";
        |    System.out.println(s);
        |  }
        |}
      """.stripMargin

    val file = makeIR(src)
    val test0 = file.modules.head
    val method = test0.procedures.head

    val body = method.asInstanceOf[IRMethod].methodBody

    val outField = loadClass("java/lang/System").toOption.flatMap(_.classModule.findField("out", test0))
    val printMethods = loadClass("java/io/PrintStream").toOption.flatMap(getObjectType(_, Nil)).map(_.findMethod("println", test0, false)).getOrElse(Nil)
    val printMethod = printMethods.find { m =>
      m.erasedParameterTypes.headOption.contains(stringClass)
    }

    val expected = IRMethodBody(IRBlock(List(
      IRLocalDeclarationStatement(IRLocalDeclaration(stringType.get, List(IRVariableDeclarator("s", 0, Some(IRStringLiteral("hello, world")))))),
      IRExpressionStatement(IRInstanceMethodCall(IRStaticFieldAccess(outField.get), Map.empty, printMethod.get, List(IRLocalVariableRef(stringType.get, "s")), Nil)))))

    body shouldBe Some(expected)
  }
}
