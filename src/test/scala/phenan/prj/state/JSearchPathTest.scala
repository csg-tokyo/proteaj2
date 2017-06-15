package phenan.prj.state

import org.scalatest._

class JSearchPathTest extends FunSuite with Matchers {
  private val path = Thread.currentThread().getContextClassLoader.getResource("proteaj/lang/PredefOperators.class").getPath.stripSuffix("proteaj/lang/PredefOperators.class")

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
    config.classPath = path

    config.configure.get.searchPath.find("phenan/prj/state/JSearchPath") shouldBe a [Some[_]]
  }

  test("ソースパスにあるjavaファイルが読める") {
    val config = JConfig()
    config.sourcePath = path + "../../../src/main/java"

    config.configure.get.searchPath.find("proteaj/lang/PredefOperators") shouldBe a [Some[_]]
  }
}
