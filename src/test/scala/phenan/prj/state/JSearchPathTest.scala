package phenan.prj.state

import org.scalatest._

class JSearchPathTest extends FunSuite with Matchers {
  test("標準ライブラリが読める") {
    val state  = JConfig().configure.get
    state.searchPath.find("java/lang/String") shouldBe a [Some[_]]
    state.clean()
  }

  test("クラスパスにないクラスは読めない") {
    val state  = JConfig().configure.get
    state.searchPath.find("phenan/jir/JClassPath") shouldBe None
    state.clean()
  }

  test("クラスパスにある自作クラスは読める") {
    val config = JConfig()
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/classes"

    config.configure.get.searchPath.find("phenan/prj/state/JSearchPath") shouldBe a [Some[_]]
  }

  test("ソースパスにあるjavaファイルが読める") {
    val config = JConfig()
    config.sourcePath = "/Users/ichikawa/workspaces/Idea/proteaj/src"

    config.configure.get.searchPath.find("proteaj/Compiler") shouldBe a [Some[_]]
  }
}
