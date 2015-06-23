package phenan.prj.internal

import org.scalatest._

import phenan.prj._
import phenan.prj.ir._
import phenan.prj.state._

import scala.util._

class JClassLoaderTest extends FunSuite with Matchers {
  test ("String 型をロード") {
    implicit val state = JConfig().configure.get
    val loader = (new JCompiler).classLoader

    val clazz = loader.load("java/lang/String")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]
  }

  test ("配列型をロード") {
    implicit val state  = JConfig().configure.get
    val loader = (new JCompiler).classLoader

    val o = loader.load("java/lang/Object")
    val os = loader.load("[Ljava/lang/Object;")
    val oss = loader.load("[[Ljava/lang/Object;")

    o shouldBe a [Success[_]]
    os shouldBe a [Success[_]]
    oss shouldBe a [Success[_]]

    loader.arrayOf(o.get) shouldBe os.get
    loader.arrayOf(os.get) shouldBe oss.get
  }

  test("ソースをJClassLoader経由でコンパイル") {
    val config = new JConfig
    config.sourcePath = "/Users/ichikawa/workspaces/Idea/prj/src/test/java"
    implicit val state = config.configure.get
    val loader = (new JCompiler).classLoader

    val clazz = loader.load("test/Hello")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [IRModule]

  }

  test("ClassSigが読めるか") {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/test-classes/"
    implicit val state = config.configure.get
    val loader = (new JCompiler).classLoader

    val clazz = loader.load("test/Var")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]

    val varClass = clazz.get.asInstanceOf[JLoadedClass]

    varClass.signature shouldBe
      JClassSignature(List(FormalMetaParameter("T", JTypeSignature.typeTypeSig, None, Nil), FormalMetaParameter("id", SimpleClassTypeSignature("proteaj/lang/Identifier", Nil), None, Nil)), JTypeSignature.objectTypeSig, Nil)

    varClass.isContext shouldBe true
  }

  test("Operatorが読めるか") {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/test-classes/"
    implicit val state = config.configure.get
    val loader = (new JCompiler).classLoader

    val clazz = loader.load("test/Var")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]

    val cl = clazz.get.asInstanceOf[JLoadedClass]
    val setterMethod = cl.methods.find(_.name == "setter")

    setterMethod shouldBe a [Some[_]]

    val method = setterMethod.get

    method.syntax shouldBe a [Some[_]]
    method.syntax.get.priority shouldBe JPriority(SimpleClassTypeSignature("test/Var", Nil), "setter")

    val pattern = method.syntax.get.syntax
    pattern shouldBe List(JMetaValueRefDef("id"), JOperatorNameDef("="), JOperandDef)
  }
}
