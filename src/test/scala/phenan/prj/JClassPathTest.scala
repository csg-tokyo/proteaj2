package phenan.prj

import org.scalatest._
import scala.util._

class JClassPathTest extends FunSuite with Matchers {
  test("デフォルトパスがとれる") {
    JClassPath.get shouldBe a [Success[_]]
  }

  test("標準ライブラリが読める") {
    val path = JClassPath.get
    path.map(cp => cp.find("java/lang/String") shouldBe a [Some[_]])
  }

  test("クラスパスにないクラスは読めない") {
    val path = JClassPath.get
    path.map(cp => cp.find("phenan/jir/JClassPath") shouldBe None)
  }

  test("クラスパスにある自作クラスは読める") {
    val config = new JirConfigBuilder
    config.classPath = "/Users/ichikawa/workspaces/Idea/prj/target/scala-2.11/classes"

    val path = JClassPath.get(config.make)
    path.map(cp => cp.find("phenan/prj/JClassPath") shouldBe a [Some[_]])
  }
}
