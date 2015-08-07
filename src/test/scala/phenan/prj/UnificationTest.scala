package phenan.prj

import org.scalatest._
import phenan.prj.state.JConfig


class UnificationTest extends FunSuite with Matchers {
  val compiler = new JCompiler(JConfig().configure.get)

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
    ssMap.findMethod("entrySet", ssMap.erase, true) should have size 1

    val entrySetMethod = ssMap.findMethod("entrySet", ssMap.erase, true).head

    val ssMapEntryType = compiler.classLoader.loadClass("java/util/Map$Entry").get.objectType(List(stringType, stringType)).get
    val setOfssMapEntryType = compiler.classLoader.loadClass("java/util/Set").get.objectType(List(ssMapEntryType)).get

    val map = setOfssMapEntryType <=< entrySetMethod.returnType

    map shouldBe Some(ssMap.env)
  }

  test ("Stream<String> s; Stream<List<String>> list = s.map(...);") {
    val stringType = compiler.classLoader.loadClass("java/lang/String").get.objectType(Nil).get
    val sStream = compiler.classLoader.loadClass("java/util/stream/Stream").get.objectType(List(stringType)).get
    sStream.findMethod("map", sStream.erase, true) should have size 1

    val mapMethod = sStream.findMethod("map", sStream.erase, true).head

    val stringListType = compiler.classLoader.loadClass("java/util/List").get.objectType(List(stringType)).get
    val sListStream = compiler.classLoader.loadClass("java/util/stream/Stream").get.objectType(List(stringListType)).get

    val map = sListStream <=< mapMethod.returnType

    map shouldBe Some(sStream.env + ("R" -> stringListType))
  }
}
