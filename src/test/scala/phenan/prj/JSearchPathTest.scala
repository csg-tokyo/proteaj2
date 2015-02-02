package phenan.prj

import org.scalatest._
import phenan.prj.config._

class JSearchPathTest extends FunSuite with Matchers {
  test("標準ライブラリが読める") {
    JConfig.default.classPath.findClassFile("java/lang/String") shouldBe a [Some[_]]
  }

  test("クラスパスにないクラスは読めない") {
    JConfig.default.classPath.findClassFile("phenan/jir/JClassPath") shouldBe None
  }

  test("クラスパスにある自作クラスは読める") {
    val config = new JConfigBuilder
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/classes"

    config.make.classPath.findClassFile("phenan/prj/config/JSearchPath") shouldBe a [Some[_]]
  }
}
