package phenan.prj.internal

import org.scalatest._

import phenan.prj._
import phenan.prj.ir.IRClass
import phenan.prj.state._

import scala.util._

class JClassLoaderTest extends FunSuite with Matchers {
  test ("String 型をロード") {
    implicit val state  = JConfig().configure.get
    val loader = (new JCompiler).loader

    val clazz = loader.load("java/lang/String")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]
  }

  test ("配列型をロード") {
    implicit val state  = JConfig().configure.get
    val loader = (new JCompiler).loader

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
    val loader = (new JCompiler).loader

    val clazz = loader.load("test/Hello")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [IRClass]

  }

  test("SimpleDSLClass") {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/test-classes/"
    implicit val state = config.configure.get
    val loader = (new JCompiler).loader

    val clazz = loader.load("test/SimpleDSLClass")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]

    val annotations = clazz.get.asInstanceOf[JLoadedClass].annotations
    annotations.dsl shouldBe a [Some[_]]
    annotations.signature shouldBe None

    annotations.dsl.get.priorities shouldBe Nil
    annotations.dsl.get.withDSLs shouldBe Nil
  }

  test("ClassSigが読めるか") {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/test-classes/"
    implicit val state = config.configure.get
    val loader = (new JCompiler).loader

    val clazz = loader.load("test/Var")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]

    val varClass = clazz.get.asInstanceOf[JLoadedClass]

    varClass.signature shouldBe Some(
      ClassSignature(List(FormalMetaParameter("T", TypeSignature.typeTypeSig, Nil), FormalMetaParameter("id", SimpleClassTypeSignature("proteaj/lang/Identifier", Nil), Nil)), TypeSignature.objectTypeSig, Nil))

    varClass.isContext shouldBe true
  }

  test("Operatorが読めるか") {
    val config = new JConfig
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/test-classes/"
    implicit val state = config.configure.get
    val loader = (new JCompiler).loader

    val clazz = loader.load("test/Var")
    clazz shouldBe a [Success[_]]
    clazz.get shouldBe a [JLoadedClass]

    val cl = clazz.get.asInstanceOf[JLoadedClass]
    val setterMethod = cl.methods.find(_.name == "setter")

    setterMethod shouldBe a [Some[_]]

    val annotations = setterMethod.get.annotations
    annotations.signature shouldBe None
    annotations.operator shouldBe a [Some[_]]

    annotations.operator.get shouldBe a [PrjExpressionOperator]
    annotations.operator.get.association shouldBe a [PrjNonAssociation]

    val pattern = annotations.operator.get.pattern
    pattern shouldBe List(PrjOperatorPureValueRef("id"), PrjOperatorName("="), PrjOperatorHole)
  }
}
