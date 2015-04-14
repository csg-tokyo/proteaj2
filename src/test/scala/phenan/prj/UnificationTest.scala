package phenan.prj

import org.scalatest._
import phenan.prj.state.JConfig


class UnificationTest extends FunSuite with Matchers {
  implicit val state = JConfig().configure.get
  val compiler = new JCompiler()

  test ("List<String> <=< List<T> ") {
    val stringType = compiler.classLoader.loadClass("java/lang/String").get.objectType(Nil).get
    val stringListType = compiler.classLoader.loadClass("java/util/List").get.objectType(List(stringType)).get
    val listSig = SimpleClassTypeSignature("java/util/List", List(JTypeVariableSignature("T")))
    val map = stringListType <=< JGenericType(listSig, Map.empty, compiler)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("List<String> <=< ArrayList<T>") {
    val stringType = compiler.classLoader.loadClass("java/lang/String").get.objectType(Nil).get
    val stringListType = compiler.classLoader.loadClass("java/util/List").get.objectType(List(stringType)).get
    val listSig = SimpleClassTypeSignature("java/util/ArrayList", List(JTypeVariableSignature("T")))
    val map = stringListType <=< JGenericType(listSig, Map.empty, compiler)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("List<?> <=< ArrayList<T>") {
    val anyListType = compiler.classLoader.loadClass("java/util/List").get.objectType(List(JWildcard(None, None))).get
    val listSig = SimpleClassTypeSignature("java/util/ArrayList", List(JTypeVariableSignature("T")))
    val map = anyListType <=< JGenericType(listSig, Map.empty, compiler)

    map shouldBe Some(Map("T" -> JWildcard(None, None)))
  }

  test ("Map<String, String> m; Set<Map.Entry<String, String>> set = m.entrySet();") {
    val stringType = compiler.classLoader.loadClass("java/lang/String").get.objectType(Nil).get
    val ssMap = compiler.classLoader.loadClass("java/util/Map").get.objectType(List(stringType, stringType)).get
    ssMap.methods.get("entrySet") shouldBe a [Some[_]]

  }
}
