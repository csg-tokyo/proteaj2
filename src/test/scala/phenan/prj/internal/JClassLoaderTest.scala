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

  val defaultCompiler = JCompiler(Config())
  val compiler2 = JCompiler(Config().copy(sourcePath = Stream(DirectoryPath(new File(testSourcePath)))))
  val compiler3 = JCompiler(Config().copy(classPath = Stream(DirectoryPath(new File(testClassPath)))))

  test ("String 型をロード") {
    import defaultCompiler._

    val clazz = load("java/lang/String")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JClass]
  }

  test ("proteaj/impl/DSL をロード") {
    import defaultCompiler._
    /*val config = JConfig()
    config.classPath = classPath
    val loader = new JCompiler(config.configure.get).classLoader
    */
    val clazz = load("proteaj/impl/DSL")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JClass]
  }

  test ("proteaj/lang/Type はロードできない") {
    import defaultCompiler._
    /*val config = JConfig()
    config.classPath = classPath
    val loader = new JCompiler(config.configure.get).classLoader*/

    val clazz = load("proteaj/lang/Type")
    clazz shouldBe a [Failure[_]]
  }

  test ("配列型をロード") {
    import defaultCompiler._

    val o = load("java/lang/Object")
    val os = load("[Ljava/lang/Object;")
    val oss = load("[[Ljava/lang/Object;")

    o shouldBe a [Success[_]]
    os shouldBe a [Success[_]]
    oss shouldBe a [Success[_]]

    arrayClassOf(o.get) shouldBe os.get
    arrayClassOf(os.get) shouldBe oss.get
  }

  test("ソースをJClassLoader経由でコンパイル") {
    import compiler2._

    val clazz = load("test/Hello")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [IRModule]

  }

  test("ClassSigが読めるか") {
    import compiler3._

    val clazz = load("test/Var")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JClass]

    val varClass = clazz.get.asInstanceOf[JClass]

    varClass.signature shouldBe
      JClassSignature(List(FormalMetaParameter("T", JTypeSignature.typeTypeSig, Nil), FormalMetaParameter("id", SimpleClassTypeSignature("proteaj/lang/Identifier", Nil), Nil)), JTypeSignature.objectTypeSig, Nil)
  }

  test("Operatorが読めるか") {
    import compiler3._

    val clazz = load("test/Var")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JClass]

    val cl = clazz.get.asInstanceOf[JClass]
    val setterMethod = cl.methods.find(_.name == "setter")

    setterMethod shouldBe a [Some[_]]

    val method = setterMethod.get

    method.syntax shouldBe a [Some[_]]
    method.syntax.get.priority shouldBe JPriority(SimpleClassTypeSignature("test/Var", Nil), "setter")

    val pattern = method.syntax.get.syntax
    pattern shouldBe List(JMetaValueRefDef("id", None), JOperatorNameDef("="), JOperandDef(None))
  }
}
