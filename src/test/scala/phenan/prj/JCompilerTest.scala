package phenan.prj

import org.scalatest._
import phenan.prj.generator.{JavaCodeGenerators, JavaReprGenerator}
import phenan.prj.ir._
import phenan.prj.state._

class JCompilerTest extends FunSuite with Matchers with BeforeAndAfterAll {
  val config = JConfig()
  config.sourcePath = "/Users/ichikawa/workspaces/Idea/prj/src/test/java"
  val state = config.configure.get



  test ("Hello をコンパイルしてみる") {
    val compiler = new JCompiler(state)

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Hello.java"))
    val clazz = compiler.findIR("test/Hello")
    clazz shouldBe a [Some[_]]
    clazz.get shouldBe a [IRModule]

    clazz.get.methods should have (size (2))

    val repr = JavaReprGenerator.moduleDef(clazz.get)

    val expected =
      """@proteaj.lang.ClassSig(metaParameters={}, superType="Ljava/lang/Object;", interfaces={})
        |public class Hello extends java.lang.Object {
        |  @proteaj.lang.MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={})
        |  public void greet() {
        |    java.lang.System.out.println("Hello, world!");
        |  }
        |  @proteaj.lang.MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={"[Ljava/lang/String;"})
        |  public static void main(java.lang.String[] args) {
        |    new test.Hello().greet();
        |  }
        |}""".stripMargin

    JavaCodeGenerators.moduleDef(repr) shouldBe expected
  }

  test ("Foo をコンパイルしてみる") {
    val compiler = new JCompiler(state)

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Hello.java", "/Users/ichikawa/workspaces/Idea/prj/src/test/java/test/Foo.java"))
    compiler.findIR("test/Hello") shouldBe a [Some[_]]

    val foo = compiler.findIR("test/Foo")
    foo shouldBe a [Some[_]]
    foo.get shouldBe a [IRModule]

    foo.get.fields should have (size (1))

    val field = foo.get.fields.head

    val hello = compiler.findIR("test/Hello")
    hello shouldBe a [Some[_]]
    hello.get shouldBe a [IRModule]

    hello.get.methods should have (size (2))
  }

  test ("PrintDSL") {
    val compiler = new JCompiler(state)

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/print/PrintDSL.pj", "/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/print/Main.pj"))
    val clazz = compiler.findIR("print/PrintDSL")
    clazz shouldBe a [Some[_]]

    val repr1 = JavaReprGenerator.moduleDef(clazz.get)

    val expected1 =
    """@proteaj/lang/ClassSig(metaParameters={}, superType="Ljava/lang/Object;", interfaces={}) @proteaj/lang/DSL(priorities={}, constraints={}, with={})
      |class PrintDSL extends java.lang.Object {
      |  @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={"Ljava/lang/String;"}) @proteaj/lang/Operator(level=proteaj/lang/OpLevel.Expression, priority=@proteaj/lang/Priority(dsl="Lprint/PrintDSL;", name="ProteanOperatorPriority$0"), pattern={@proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Name, Name="p"), @proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Hole, Hole="")})
      |  static final void ProteanOperator$1(java.lang.String msg) {
      |    java.lang.System.out.println(msg);
      |  }
      |}""".stripMargin

    //JavaCodeGenerators.moduleDef(repr1) shouldBe expected1
    println(JavaCodeGenerators.moduleDef(repr1))

    val main = compiler.findIR("print/Main")
    main shouldBe a [Some[_]]

    val repr2 = JavaReprGenerator.moduleDef(main.get)

    val expected2 =
    """@proteaj/lang/ClassSig(metaParameters={}, superType="Ljava/lang/Object;", interfaces={})
      |public class Main extends java.lang.Object {
      |  @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={"[Ljava/lang/String;"})
      |  public static void main(java.lang.String[] args) {
      |    print.PrintDSL.ProteanOperator$1("Hello, world!");
      |  }
      |}""".stripMargin

    //JavaCodeGenerators.moduleDef(repr2) shouldBe expected2
    println(JavaCodeGenerators.moduleDef(repr2))
  }

  import scala.sys.process._

  test ("LetDSL") {
    val result = compileAndRun("let.Main", List("src/test/proteaj/let/LetDSL.pj", "src/test/proteaj/let/Main.pj"))
    result shouldBe "hello\n"
  }

  test ("FileDSL") {
    val result = compileAndRun("file.Main", List("src/test/proteaj/file/FileDSL.pj", "src/test/proteaj/file/Main.pj"))
    result shouldBe "The ProteaJ compiler\n"
  }

  test ("LetDSL2") {
    val compiler = new JCompiler(state)

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/let2/LetDSL.pj", "/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/let2/Main.pj"))

    val clazz = compiler.findIR("let2/LetDSL")
    clazz shouldBe a [Some[_]]

    val repr1 = JavaReprGenerator.moduleDef(clazz.get)
    println(JavaCodeGenerators.moduleDef(repr1))

    val main = compiler.findIR("let2/Main")
    main shouldBe a [Some[_]]

    val repr2 = JavaReprGenerator.moduleDef(main.get)
    println(JavaCodeGenerators.moduleDef(repr2))
  }

  override protected def beforeAll(): Unit = {
    if (new java.io.File("bin").isDirectory) "rm -r bin" ! ProcessLogger(_ => ())
    "mkdir bin" ! ProcessLogger(_ => ())
  }

  def compileAndRun (mainClass: String, files: List[String]): String = {
    Seq("sbt", "run -d bin " + files.mkString(" ")).!
    ( "java -cp bin:target/scala-2.11/classes " + mainClass ).!!
  }
}
