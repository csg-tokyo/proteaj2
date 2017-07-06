package phenan.prj

import org.scalatest._
import phenan.prj.state.Config

class UnificationTest extends FunSuite with Matchers {
  val compiler: JCompiler.JCompilerImpl = JCompiler.init(Config()).right.get

  import compiler._

  private def load (name: String, args: MetaArgument*): JObjectType = {
    getObjectType(loadClass(name).get, args.toList).get
  }

  test ("List<String> <=< List<T> ") {
    val stringType = load("java/lang/String")
    val stringListType = load("java/util/List", stringType)
    val listSig = SimpleClassTypeSignature("java/util/List", List(JTypeVariableSignature("T")))
    val map = stringListType <=< JGenericType(listSig, Map.empty)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("List<String> <=< ArrayList<T>") {
    val stringType = load("java/lang/String")
    val stringListType = load("java/util/List", stringType)
    val listSig = SimpleClassTypeSignature("java/util/ArrayList", List(JTypeVariableSignature("T")))
    val map = stringListType <=< JGenericType(listSig, Map.empty)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("List<?> <=< ArrayList<T>") {
    val anyListType = load("java/util/List", JWildcard(None, None))
    val listSig = SimpleClassTypeSignature("java/util/ArrayList", List(JTypeVariableSignature("T")))
    val map = anyListType <=< JGenericType(listSig, Map.empty)

    map shouldBe Some(Map("T" -> JWildcard(None, None)))
  }

  test ("Map<String, String> m; Set<Map.Entry<String, String>> set = m.entrySet();") {
    val stringType = load("java/lang/String")
    val ssMap = load("java/util/Map", stringType, stringType)
    ssMap.findMethod("entrySet", ssMap.erase, true) should have size 1

    val entrySetMethod = ssMap.findMethod("entrySet", ssMap.erase, true).head

    val ssMapEntryType = load(s"java/util/Map$$Entry", stringType, stringType)
    val setOfssMapEntryType = load("java/util/Set", ssMapEntryType)

    val map = setOfssMapEntryType <=< entrySetMethod.returnType

    map shouldBe Some(ssMap.env)
  }

  test ("Stream<String> s; Stream<List<String>> list = s.map(...);") {
    val stringType = load("java/lang/String")
    val sStream = load("java/util/stream/Stream", stringType)
    sStream.findMethod("map", sStream.erase, true) should have size 1

    val mapMethod = sStream.findMethod("map", sStream.erase, true).head

    val stringListType = load("java/util/List", stringType)
    val sListStream = load("java/util/stream/Stream", stringListType)

    val map = sListStream <=< mapMethod.returnType

    map shouldBe Some(sStream.env + ("R" -> stringListType))
  }

  test ("Map<String, A> <=< Map<T, T>") {
    val stringType = load("java/lang/String")
    val unbound = JUnboundTypeVariable("A", List(objectType))

    val saMap = load("java/util/Map", stringType, unbound)
    val ttMap = SimpleClassTypeSignature("java/util/Map", List(JTypeVariableSignature("T"), JTypeVariableSignature("T")))

    val map = saMap <=< JGenericType(ttMap, Map.empty)

    map shouldBe Some(Map("T" -> stringType))
  }

  test ("unbound <=< type parameter") {
    val unbound = JUnboundTypeVariable("S", List(objectType))
    val sig = JTypeVariableSignature("T")
    val map = MetaArgumentUnifier.check(unbound, sig, Map.empty[String, MetaArgument])

    map shouldBe Some(Map("T" -> unbound))
  }

  test ("Map<S, A> <=< Map<String, T>") {
    val unbound1 = JUnboundTypeVariable("S", List(objectType))
    val unbound2 = JUnboundTypeVariable("A", List(objectType))

    val saMap = load("java/util/Map", unbound1, unbound2)
    val stMap = SimpleClassTypeSignature("java/util/Map", List(SimpleClassTypeSignature("java/lang/String", Nil), JTypeVariableSignature("T")))

    val map = saMap <=< JGenericType(stMap, Map.empty)

    map shouldBe Some(Map("T" -> unbound2))
  }

  test ("String <=< T (T = A = unbound) ") {
    val string = load("java/lang/String")
    val unbound = JUnboundTypeVariable("A", List(objectType))
    val tv = JTypeVariableSignature("T")

    val map = string <=< JGenericType(tv, Map("T" -> unbound))

    map shouldBe Some(Map("T" -> string))
  }
}
