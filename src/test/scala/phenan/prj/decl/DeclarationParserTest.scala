package phenan.prj.decl

import java.io._

import org.scalatest._

import phenan.prj.state._

class DeclarationParserTest extends FunSuite with Matchers with ASTUtil {
  implicit val state  = JConfig().configure.get

  test ("空のプログラム") {
    val src = ""
    val ast = parse(src)

    ast shouldBe CompilationUnit(Header(None, Nil), Nil)
  }

  test ("パッケージ宣言") {
    val src = "package phenan.prj.decl;"
    val ast = parse(src)

    ast.header.pack shouldBe Some(packageDcl("phenan", "prj", "decl"))
    ast.header.imports shouldBe Nil
    ast.modules shouldBe Nil
  }

  test ("クラスインポート") {
    val src = "import java.io.Reader;"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe List(classImport("java", "io", "Reader"))
    ast.modules shouldBe Nil
  }

  test ("パッケージインポート") {
    val src = "import java.util.*;"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe List(packageImport("java", "util"))
    ast.modules shouldBe Nil
  }

  test ("静的メソッドインポート") {
    val src = "import static java.lang.Character.isDigit;"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe List(staticImport("java", "lang", "Character", "isDigit"))
    ast.modules shouldBe Nil
  }

  test ("全静的メンバインポート") {
    val src = "import static java.lang.Character.*;"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe List(staticImportAll("java", "lang", "Character"))
    ast.modules shouldBe Nil
  }

  test ("DSLインポート") {
    val src =
      """
        |import dsl phenan.Matrix;
        |import dsl phenan.SQL > phenan.File;
        |import dsl phenan.Regex < phenan.Grep < phenan.Test;
        |""".stripMargin
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe List(
      dslImport("phenan", "Matrix"),
      dslImport("phenan", "SQL") > qualifiedName("phenan", "File"),
      dslImport("phenan", "Regex") < qualifiedName("phenan", "Grep") < qualifiedName("phenan", "Test"))
    ast.modules shouldBe Nil
  }

  test ("単純なクラス宣言") {
    val src = "public class Main {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Main", Nil, None, Nil, Nil))
  }

  test ("マーカーアノテーション") {
    val src = "@Test public class Main {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(MarkerAnnotation(qualifiedName("Test")), PublicModifier), "Main", Nil, None, Nil, Nil))
  }

  test ("単一引数アノテーション") {
    val src = "public @SingleArg(Foo.class) class Main {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier, SingleElementAnnotation(qualifiedName("SingleArg"), expression("Foo.class", 1))), "Main", Nil, None, Nil, Nil))
  }

  test ("フルアノテーション") {
    val src =
      """@full.Annotation(numArgs = (1 + 1), value = {"foo", @Sub, "bar"})
        |public class Main {}""".stripMargin
    val ast = parse(src)

    val ann = FullAnnotation(qualifiedName("full", "Annotation"), Map("numArgs" -> expression(" (1 + 1)", 1), "value" -> arrayOf(expression("\"foo\"", 1), MarkerAnnotation(qualifiedName("Sub")), expression(" \"bar\"", 1))))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(ann, PublicModifier), "Main", Nil, None, Nil, Nil))
  }

  test ("extends 節付きのクラス宣言") {
    val src = "public class Main extends java.awt.event.MouseAdapter {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Main", Nil, Some(simpleType("java", "awt", "event", "MouseAdapter")), Nil, Nil))
  }

  test ("implements 節付きのクラス宣言") {
    val src = "public class Main implements Cloneable, Serializable {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Main", Nil, None, List(simpleType("Cloneable"), simpleType("Serializable")), Nil))
  }

  test ("型引数付きのクラス宣言") {
    val src = "public class MyList <T> extends AbstractList<T> implements List<T> {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "MyList", List(TypeParameter("T", Nil)),
      Some(ClassTypeName(qualifiedName("AbstractList"), List(simpleType("T")))),
      List(ClassTypeName(qualifiedName("List"), List(simpleType("T")))), Nil))
  }

  test ("複雑な型引数") {
    val src = "public class Foo <S extends java.lang.Object, T extends Bar<?> & Baz<? extends S>> extends Fizz<S, ? super T> {}"
    val ast = parse(src)

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil

    val typeParams = List(
      TypeParameter("S", List(simpleType("java", "lang", "Object"))),
      TypeParameter("T", List(ClassTypeName(qualifiedName("Bar"), List(WildcardType(None, None))), ClassTypeName(qualifiedName("Baz"), List(WildcardType(Some(simpleType("S")), None))))))
    val superType = Some(
      ClassTypeName(qualifiedName("Fizz"), List(simpleType("S"), WildcardType(None, Some(simpleType("T"))))))
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", typeParams, superType, Nil, Nil))
  }

  test ("インスタンスイニシャライザ") {
    val src =
      """public class Foo {
        |  { System.out.println("Hello, world!"); }
        |}
      """.stripMargin
    val ast = parse(src)

    val member = InstanceInitializer(block(" System.out.println(\"Hello, world!\"); ", 2))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test ("スタティックイニシャライザ") {
    val src =
      """public class Foo {
        |  static { System.out.println("Hello, world!"); }
        |}
      """.stripMargin
    val ast = parse(src)

    val member = StaticInitializer(block(" System.out.println(\"Hello, world!\"); ", 2))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test ("単純なフィールド宣言") {
    val src =
      """public class Foo {
        |  private static final int zero = 0;
        |}
      """.stripMargin
    val ast = parse(src)

    val member = FieldDeclaration(List(PrivateModifier, StaticModifier, FinalModifier), simpleType("int"), List(VariableDeclarator("zero", 0, Some(expression(" 0", 2)))))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test ("複雑なフィールド宣言") {
    val src =
      """public class Foo {
        |  protected static final String comma = ",", braces[] = {"{", "}"};
        |  private java.lang.StringBuilder buf1, buf2 = new StringBuilder();
        |}
      """.stripMargin
    val ast = parse(src)

    val member1 = FieldDeclaration(List(ProtectedModifier, StaticModifier, FinalModifier), simpleType("String"), List(
      VariableDeclarator("comma", 0, Some(expression(" \",\"", 2))),
      VariableDeclarator("braces", 1, Some(expression(" {\"{\", \"}\"}", 2)))))
    val member2 = FieldDeclaration(List(PrivateModifier), simpleType("java", "lang", "StringBuilder"), List(
      VariableDeclarator("buf1", 0, None),
      VariableDeclarator("buf2", 0, Some(expression(" new StringBuilder()", 3)))))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member1, member2)))
  }

  test ("単純なコンストラクタ宣言") {
    val src =
      """public class Foo {
        |  Foo() {}
        |}
      """.stripMargin
    val ast = parse(src)

    val member = ConstructorDeclaration(Nil, Nil, Nil, Nil, block("", 2))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test ("複雑なコンストラクタ宣言") {
    val src =
      """public class Foo {
        |  protected <T> Foo(pure final T... ts, @Annotation List<T> list) throws Exception {
        |    if (list.containsAll(Arrays.asList(ts))) System.out.println("contains");
        |  }
        |}
      """.stripMargin

    val body =
      """
        |    if (list.containsAll(Arrays.asList(ts))) System.out.println("contains");
        |  """.stripMargin

    val ast = parse(src)

    val member = ConstructorDeclaration(List(ProtectedModifier), List(TypeParameter("T", Nil)),
      List(FormalParameter(List(PureModifier, FinalModifier), simpleType("T"), true, "ts", 0, None),
        FormalParameter(List(MarkerAnnotation(qualifiedName("Annotation"))), ClassTypeName(qualifiedName("List"), List(simpleType("T"))), false, "list", 0, None)), List(simpleType("Exception")), block(body, 2))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test ("単純なメソッド宣言") {
    val src =
      """public class Foo {
        |  public static void main (String[] args) {
        |    System.out.println("Hello, world!");
        |  }
        |}
      """.stripMargin

    val body =
      """
        |    System.out.println("Hello, world!");
        |  """.stripMargin

    val ast = parse(src)

    val member = MethodDeclaration(List(PublicModifier, StaticModifier), Nil, simpleType("void"), "main", List(FormalParameter(Nil, ArrayTypeName(simpleType("String")), false, "args", 0, None)), Nil, Some(block(body, 2)))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test ("複雑なメソッド宣言") {
    val src =
      """public class Foo {
        |  public <T, R> R map (T array[], Function<T, R> func) throws NullPointerException, FunctionCannotAppliedException {
        |    R[] rs = new R[array.length];
        |    for (int i = 0; i < array.length; i++) { rs[i] = func(array[i]); }
        |    return rs;
        |  }
        |}
      """.stripMargin

    val body =
      """
        |    R[] rs = new R[array.length];
        |    for (int i = 0; i < array.length; i++) { rs[i] = func(array[i]); }
        |    return rs;
        |  """.stripMargin

    val ast = parse(src)

    val member = MethodDeclaration(List(PublicModifier), List(TypeParameter("T", Nil), TypeParameter("R", Nil)), simpleType("R"), "map",
      List(FormalParameter(Nil, simpleType("T"), false, "array", 1, None),
        FormalParameter(Nil, ClassTypeName(qualifiedName("Function"), List(simpleType("T"), simpleType("R"))), false, "func", 0, None)),
      List(simpleType("NullPointerException"), simpleType("FunctionCannotAppliedException")), Some(block(body, 2)))

    ast.header.pack shouldBe None
    ast.header.imports shouldBe Nil
    ast.modules shouldBe List(ClassDeclaration(List(PublicModifier), "Foo", Nil, None, Nil, List(member)))
  }

  test("正常終了したか") {
    state.clean()
    state.errors shouldBe 0
    state.warns shouldBe 0
  }

  def parse (src: String): CompilationUnit = {
    val reader = new StringReader(src)
    val parser = DeclarationParser(reader, "test_source")
    parser.get.parseAll
  }
}
