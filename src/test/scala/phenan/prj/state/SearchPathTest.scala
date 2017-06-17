package phenan.prj.state

import java.io.File

import org.scalatest._

class SearchPathTest extends FunSuite with Matchers {
  private val classPath = Thread.currentThread().getContextClassLoader.getResource("proteaj/lang/PredefOperators.class").getPath.stripSuffix("proteaj/lang/PredefOperators.class")
  private val testClassPath = classPath + "../test-classes"
  private val testSourcePath = classPath + "../../../src/test/java"

  test("標準ライブラリが読める") {
    val config = Config()
    config.findClassFile("java/lang/String") shouldBe a [Some[_]]
  }

  test("クラスパスにないクラスは読めない") {
    val config = Config()
    config.findClassFile("test/Hello") shouldBe None
  }

  test("クラスパスにある自作クラスは読める") {
    val config = Config().copy(classPath = Stream(DirectoryPath(new File(testClassPath))))
    config.findClassFile("test/Hello") shouldBe a [Some[_]]
  }

  test("ソースパスにあるjavaファイルが読める") {
    val config = Config().copy(sourcePath = Stream(DirectoryPath(new File(testSourcePath))))
    config.findSourceFile("test/Hello") shouldBe a [Some[_]]
  }
}
