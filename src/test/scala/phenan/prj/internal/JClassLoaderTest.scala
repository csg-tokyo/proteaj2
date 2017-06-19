package phenan.prj.internal

import java.io.File

import org.scalatest._
import phenan.prj._
import phenan.prj.state._

import scala.util._

class JClassLoaderTest extends FunSuite with Matchers {
  private val classPath = Thread.currentThread().getContextClassLoader.getResource("proteaj/lang/PredefOperators.class").getPath.stripSuffix("proteaj/lang/PredefOperators.class")
  private val testClassPath = classPath + "../test-classes"
  private val testSourcePath = classPath + "../../../src/test/java"

  val defaultCompiler: JCompiler.JCompilerImpl = JCompiler.init(Config()).right.get
  val compiler2: JCompiler.JCompilerImpl = JCompiler.init(Config().copy(sourcePath = Stream(DirectoryPath(new File(testSourcePath))))).right.get
  val compiler3: JCompiler.JCompilerImpl = JCompiler.init(Config().copy(classPath = Stream(DirectoryPath(new File(testClassPath))))).right.get

  test ("String 型をロード") {
    import defaultCompiler._

    val clazz = loadClass("java/lang/String")
    clazz shouldBe a [Success[_]]
  }

  test ("proteaj/impl/DSL をロード") {
    import defaultCompiler._

    val clazz = loadClass("proteaj/impl/DSL")
    clazz shouldBe a [Success[_]]
  }

  test ("proteaj/lang/Type はロードできない") {
    import defaultCompiler._

    val clazz = loadClass("proteaj/lang/Type")
    clazz shouldBe a [Failure[_]]
  }

  test("ソースをJClassLoader経由でコンパイル") {
    import compiler2._

    val clazz = loadClass("test/Hello")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [IRModule]

  }

  test("ClassSigが読めるか") {
    import compiler3._

    val clazz = loadClass("test/Var")
    clazz shouldBe a [Success[_]]

    val varClass = clazz.get

    varClass.signature shouldBe
      JClassSignature(List(FormalMetaParameter("T", JTypeSignature.typeTypeSig, Nil), FormalMetaParameter("id", SimpleClassTypeSignature("proteaj/lang/Identifier", Nil), Nil)), JTypeSignature.objectTypeSig, Nil)
  }

  test("Operatorが読めるか") {
    import compiler3._

    val clazz = loadClass("test/Var")
    clazz shouldBe a [Success[_]]

    val cl = clazz.get
    val setterMethod = cl.methods.find(_.name == "setter")

    setterMethod shouldBe a [Some[_]]

    val method = setterMethod.get

    method.syntax shouldBe a [Some[_]]
    method.syntax.get.priority shouldBe JPriority(SimpleClassTypeSignature("test/Var", Nil), "setter")

    val pattern = method.syntax.get.syntax
    pattern shouldBe List(JMetaValueRefDef("id", None), JOperatorNameDef("="), JOperandDef(None))
  }
}
