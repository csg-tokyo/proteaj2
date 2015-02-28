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

    val ir = clazz.get.asInstanceOf[IRClass]

  }
}
