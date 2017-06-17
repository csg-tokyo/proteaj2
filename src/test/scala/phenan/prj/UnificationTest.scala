package phenan.prj

import org.scalatest._
import phenan.prj.state.Config

class UnificationTest extends FunSuite with Matchers {
  val compiler = JCompiler(Config())

  import compiler._

  test ("List<String> <=< List<T> ") {
    val stringType = loadClass("java/lang/String").get.objectType(Nil).get
    val stringListType = loadClass("java/util/List").get.objectType(List(stringType)).get
    val listSig = SimpleClassTypeSignature("java/util/List", List(JTypeVariableSignature("T")))
    val map = stringListType <=< JGenericType(listSig, Map.empty)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("List<String> <=< ArrayList<T>") {
    val stringType = loadClass("java/lang/String").get.objectType(Nil).get
    val stringListType = loadClass("java/util/List").get.objectType(List(stringType)).get
    val listSig = SimpleClassTypeSignature("java/util/ArrayList", List(JTypeVariableSignature("T")))
    val map = stringListType <=< JGenericType(listSig, Map.empty)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("List<?> <=< ArrayList<T>") {
    val anyListType = loadClass("java/util/List").get.objectType(List(JWildcard(None, None))).get
    val listSig = SimpleClassTypeSignature("java/util/ArrayList", List(JTypeVariableSignature("T")))
    val map = anyListType <=< JGenericType(listSig, Map.empty)

    map shouldBe Some(Map("T" -> JWildcard(None, None)))
  }

  test ("Map<String, String> m; Set<Map.Entry<String, String>> set = m.entrySet();") {
    val stringType = loadClass("java/lang/String").get.objectType(Nil).get
    val ssMap = loadClass("java/util/Map").get.objectType(List(stringType, stringType)).get
    ssMap.findMethod("entrySet", ssMap.erase, true) should have size 1

    val entrySetMethod = ssMap.findMethod("entrySet", ssMap.erase, true).head

    val ssMapEntryType = loadClass(s"java/util/Map$$Entry").get.objectType(List(stringType, stringType)).get
    val setOfssMapEntryType = loadClass("java/util/Set").get.objectType(List(ssMapEntryType)).get

    val map = setOfssMapEntryType <=< entrySetMethod.returnType

    map shouldBe Some(ssMap.env)
  }

  test ("Stream<String> s; Stream<List<String>> list = s.map(...);") {
    val stringType = loadClass("java/lang/String").get.objectType(Nil).get
    val sStream = loadClass("java/util/stream/Stream").get.objectType(List(stringType)).get
    sStream.findMethod("map", sStream.erase, true) should have size 1

    val mapMethod = sStream.findMethod("map", sStream.erase, true).head

    val stringListType = loadClass("java/util/List").get.objectType(List(stringType)).get
    val sListStream = loadClass("java/util/stream/Stream").get.objectType(List(stringListType)).get

    val map = sListStream <=< mapMethod.returnType

    map shouldBe Some(sStream.env + ("R" -> stringListType))
  }

  test ("Map<String, A> <=< Map<T, T>") {
    val stringType = loadClass("java/lang/String").get.objectType(Nil).get
    val unbound = JUnboundTypeVariable("A", objectType.toList)

    val saMap = loadClass("java/util/Map").get.objectType(List(stringType, unbound)).get
    val ttMap = SimpleClassTypeSignature("java/util/Map", List(JTypeVariableSignature("T"), JTypeVariableSignature("T")))

    val map = saMap <=< JGenericType(ttMap, Map.empty)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("unbound <=< type parameter") {
    val unbound = JUnboundTypeVariable("S", objectType.toList)
    val sig = JTypeVariableSignature("T")
    val map = MetaArgumentUnifier.check(unbound, sig, Map.empty[String, MetaArgument])

    map shouldBe Some(Map("T" -> unbound))
  }

  test ("Map<S, A> <=< Map<String, T>") {
    val unbound1 = JUnboundTypeVariable("S", objectType.toList)
    val unbound2 = JUnboundTypeVariable("A", objectType.toList)

    val saMap = loadClass("java/util/Map").get.objectType(List(unbound1, unbound2)).get
    val stMap = SimpleClassTypeSignature("java/util/Map", List(SimpleClassTypeSignature("java/lang/String", Nil), JTypeVariableSignature("T")))

    val map = saMap <=< JGenericType(stMap, Map.empty)

    map shouldBe Some(Map("T" -> unbound2))
  }

  test ("String <=< T (T = A = unbound) ") {
    val string = loadClass("java/lang/String").get.objectType(Nil).get
    val unbound = JUnboundTypeVariable("A", objectType.toList)
    val tv = JTypeVariableSignature("T")

    val map = string <=< JGenericType(tv, Map("T" -> unbound))

    map shouldBe Some(Map("T" -> string))
  }
}
