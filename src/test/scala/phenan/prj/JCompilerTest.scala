package phenan.prj

import org.scalatest._
import phenan.prj.generator.{JavaCodeGenerators, JavaReprGenerator}
import phenan.prj.ir._
import phenan.prj.state._

class JCompilerTest extends FunSuite with Matchers {
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
      """@proteaj/lang/ClassSig(metaParameters={}, superType="Ljava/lang/Object;", interfaces={})
        |public class Hello extends java.lang.Object {
        |  @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={})
        |  public void greet() {
        |    java.lang.System.out.println("Hello, world!");
        |  }
        |  @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={"[Ljava/lang/String;"})
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

  test ("LetDSL") {
    val compiler = new JCompiler(state)

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/let/LetDSL.pj", "/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/let/Main.pj"))

    val clazz = compiler.findIR("let/LetDSL")
    clazz shouldBe a [Some[_]]

    val repr1 = JavaReprGenerator.moduleDef(clazz.get)

    val expected1 =
    """@proteaj/lang/ClassSig(metaParameters={}, superType="Ljava/lang/Object;", interfaces={}) @proteaj/lang/DSL(priorities={}, constraints={}, with={})
      |class LetDSL extends java.lang.Object {
      |  @proteaj/lang/MethodSig(metaParameters={@proteaj/lang/MetaParameter(name="R", type="Lproteaj/lang/Type;", priority={}, bounds={})}, throwsTypes={}, deactivates={}, returnType="TR;", requires={}, activates={}, parameters={"Ljava/lang/String;","@Llet/LetDSL$Local<Ljava/lang/String;>;TR;"}) @proteaj/lang/Operator(level=proteaj/lang/OpLevel.Expression, priority=@proteaj/lang/Priority(dsl="Llet/LetDSL;", name="ProteanOperatorPriority$2"), pattern={@proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Name, Name="let"), @proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Name, Name="a"), @proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Name, Name="="), @proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Hole, Hole=""), @proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Name, Name="in"), @proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Hole, Hole="")})
      |  static final<R> R ProteanOperator$3(java.lang.String v, java.util.function.Function<let.LetDSL.Local<java.lang.String>, R> r) {
      |    return r.apply(new let.LetDSL.Local<java.lang.String>(v));
      |  }
      |  @proteaj/lang/ClassSig(metaParameters={@proteaj/lang/MetaParameter(name="V", type="Lproteaj/lang/Type;", priority={}, bounds={})}, superType="Ljava/lang/Object;", interfaces={}) @proteaj/lang/Context
      |  static class Local<V> extends java.lang.Object {
      |    @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={"TV;"})
      |    Local(V v) {
      |      let.LetDSL.Local.this.v=v;
      |    }
      |    @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="TV;", requires={}, activates={}, parameters={}) @proteaj/lang/Operator(level=proteaj/lang/OpLevel.Expression, priority=@proteaj/lang/Priority(dsl="Llet/LetDSL;", name="ProteanOperatorPriority$5"), pattern={@proteaj/lang/OpElem(kind=proteaj/lang/OpElemType.Name, Name="a")})
      |    V ProteanOperator$4() {
      |      return let.LetDSL.Local.this.v;
      |    }
      |    @proteaj/lang/FieldSig(value="TV;")
      |    private V v;
      |  }
      |}""".stripMargin

    println(JavaCodeGenerators.moduleDef(repr1)) // shouldBe expected1

    val main = compiler.findIR("let/Main")
    main shouldBe a [Some[_]]

    val repr2 = JavaReprGenerator.moduleDef(main.get)

    val expected2 =
    """@proteaj/lang/ClassSig(metaParameters={}, superType="Ljava/lang/Object;", interfaces={})
      |public class Main extends java.lang.Object {
      |  @proteaj/lang/MethodSig(metaParameters={}, throwsTypes={}, deactivates={}, returnType="V", requires={}, activates={}, parameters={"[Ljava/lang/String;"})
      |  public static void main(java.lang.String[] args) {
      |    java.lang.String s=let.LetDSL.<java.lang.String> ProteanOperator$3("hello", new java.util.function.Function<let.LetDSL.Local<java.lang.String>, java.lang.String>() {
      |      public java.lang.String apply(let.LetDSL.Local<java.lang.String> ProteaJLocalContext$$0) {
      |        return ProteaJLocalContext$$0.ProteanOperator$4();
      |      }
      |    });
      |  }
      |}""".stripMargin

    println(JavaCodeGenerators.moduleDef(repr2)) // shouldBe expected2
  }

  test ("FileDSL") {
    val compiler = new JCompiler(state)

    compiler.generateIR(List("/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/file/FileDSL.pj", "/Users/ichikawa/workspaces/Idea/prj/src/test/proteaj/file/Main.pj"))

    val clazz = compiler.findIR("file/FileDSL")
    clazz shouldBe a [Some[_]]

    val repr1 = JavaReprGenerator.moduleDef(clazz.get)
    // println(JavaCodeGenerators.moduleDef(repr1))

    val main = compiler.findIR("file/Main")
    main shouldBe a [Some[_]]

    val repr2 = JavaReprGenerator.moduleDef(main.get)
    // println(JavaCodeGenerators.moduleDef(repr2))
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
}
