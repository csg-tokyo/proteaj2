package phenan.prj.declaration

import org.scalatest._

class DeclarationParsersTest extends FunSuite with Matchers with ASTUtil {
  import DeclarationParsers._

  test ("package declaration") {
    val r = parse(packageDeclaration, "package phenan.prj.declaration;")
    assert(r.successful)
    r.get shouldBe PackageDeclaration(qName("phenan", "prj", "declaration"))
  }

  test ("import class declaration") {
    val r = parse(importClass, "import java.util.ArrayList;")
    assert(r.successful)
    r.get shouldBe ClassImportDeclaration(qName("java", "util", "ArrayList"))
  }

  test ("import package declaration") {
    val r = parse(importPackage, "import java.util.*;")
    assert(r.successful)
    r.get shouldBe PackageImportDeclaration(qName("java", "util"))
  }

  test ("import static member") {
    val r = parse(importStaticMember, "import static java.lang.Character.digit;")
    assert(r.successful)
    r.get shouldBe ImportStaticMemberDeclaration(qName("java", "lang", "Character", "digit"))
  }

  test ("import static star") {
    val r = parse(importStaticStar, "import static org.junit.Assert.*;")
    assert(r.successful)
    r.get shouldBe ImportStaticOnDemandDeclaration(qName("org", "junit", "Assert"))
  }

  test ("import dsl") {
    val r = parse(importDSLs, "import dsl sql.DSL;")
    assert(r.successful)
    r.get shouldBe ImportDSLsDeclaration(List(qName("sql", "DSL")), Nil)
  }

  test ("marker annotation") {
    val r = parse(markerAnnotation, "@Deprecated")
    assert(r.successful)
    r.get shouldBe MarkerAnnotation(qName("Deprecated"))
  }

  test ("single element annotation") {
    val r = parse(singleElementAnnotation, "@SuppressWarnings(\"unchecked\")")
    assert(r.successful)
    r.get shouldBe SingleElementAnnotation(qName("SuppressWarnings"), StringLiteralExpression("unchecked"))
  }

  test ("block") {
    val src = """{
                |  System.out.println("Hello, world!");
                |  return 0;
                |}""".stripMargin
    val r = parse(block, src)
    assert(r.successful)
    r.get should matchPattern { case BlockSnippet(s) if src == s => }
  }

  test ("complicated block") {
    val src = """{
                |  if (ch == '{') {
                |    System.out.println("{ Hello, world! }");
                |  }
                |  return 0;  // {
                |}""".stripMargin
    val ans = """{
                |  if (ch == '{') {
                |    System.out.println("{ Hello, world! }");
                |  }
                |  return 0;""".stripMargin + "  \n}"
    val r = parse(block, src)
    assert(r.successful)
    r.get should matchPattern { case BlockSnippet(s) if ans == s => }
  }

  test ("type parameters") {
    val src = "<T>"
    val r = parse(metaParameters, src)
    assert(r.successful)
    r.get shouldBe List(TypeParameter("T", Nil))
  }

  test ("context declaration") {
    val src = "context Var <T> {}"
    val r = parse(contextDeclaration, src)
    assert(r.successful)
    r.get shouldBe ContextDeclaration(Nil, "Var", List(TypeParameter("T", Nil)), Nil)
  }
}

trait ASTUtil {
  def qName (names: String*) = QualifiedName(names.toList)

}
