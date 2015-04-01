package phenan.prj

sealed trait MetaValue {
  def matches (v: MetaValue): Boolean
}

sealed trait PureValue extends MetaValue {
  def valueType: JType
}

case class UnknownPureValue (valueType: JType) extends PureValue {
  def matches (v: MetaValue): Boolean = false
}

case class ConcretePureValue (value: Any, valueType: JType) extends PureValue {
  override def matches(v: MetaValue): Boolean = this == v
}

sealed trait JModule {
  def fields: Map[String, JField]
  def methods: Map[String, List[JMethod]]

  def compiler: JCompiler
}

trait JGenericType {
  def signature: JTypeSignature
  def env: Map[String, MetaValue]

  def bind(args: Map[String, MetaValue]): Option[JType]
  def >=> (t: JType): Option[Map[String, MetaValue]]
  def <=< (t: JType): Option[Map[String, MetaValue]]
  def <=> (t: JType): Option[Map[String, MetaValue]]
}

case class JClassModule (clazz: JClass) extends JModule {
  def fields: Map[String, JField] = ???
  def methods: Map[String, List[JMethod]] = ???

  def compiler = clazz.compiler
}

sealed trait JType extends JModule {
  def name: String
  def array: JArrayType = compiler.typeLoader.arrayOf(this)

  def isSubtypeOf (that: JType): Boolean
  def isAssignableTo (that: JType): Boolean

  def <:< (t: JType): Boolean = this.isSubtypeOf(t)
  def >:> (t: JType): Boolean = t.isSubtypeOf(this)
}

sealed trait JRefType extends JType with MetaValue

case class JObjectType (erase: JClass, env: Map[String, MetaValue]) extends JRefType {
  def compiler = erase.compiler

  override def name: String = {
    if (env.isEmpty) erase.name
    else erase.name + env.map(kv => kv._1 + "=" + kv._2).mkString("<", ",", ">")
  }

  lazy val superType: Option[JObjectType] = erase.signature match {
    case Some(sig) => compiler.typeLoader.fromClassTypeSignature(sig.superClass, env)
    case None      => erase.superClass.flatMap(_.objectType(Nil))
  }

  lazy val interfaceTypes: List[JObjectType] = erase.signature match {
    case Some(sig) => sig.interfaces.flatMap(compiler.typeLoader.fromClassTypeSignature(_, env))
    case None      => erase.interfaces.flatMap(_.objectType(Nil))
  }

  lazy val constructors: List[JConstructor] = {
    erase.methods.filter(_.isConstructor).map { constructorDef => new JConstructor(constructorDef, env, this) }
  }

  lazy val declaredFields: List[JField] = {
    erase.fields.filterNot(_.isStatic).flatMap { fieldDef =>
      fieldDef.signature.flatMap(sig => compiler.typeLoader.fromTypeSignature(sig, env)).map(fieldType => new JField(fieldDef, fieldType, this))
    }
  }

  lazy val declaredMethods: List[JMethod] = {
    erase.methods.filter(_.isInstanceMethod).map { methodDef => new JMethod(methodDef, env, this) }
  }

  lazy val privateFields: Map[String, JField] = declaredFields.filter(_.isPrivate).map(f => f.name -> f).toMap
  lazy val privateMethods: Map[String, List[JMethod]] = declaredMethods.filter(_.isPrivate).groupBy(_.name)

  lazy val fields: Map[String, JField] = nonPrivateFieldList.map(f => f.name -> f).toMap
  lazy val methods: Map[String, List[JMethod]] = nonPrivateMethodList.groupBy(_.name).mapValues(filterOutOverriddenMethod)

  def isSubtypeOf (that: JType): Boolean = that match {
    case _ if this == that   => true
    case that: JObjectType   => isSubtypeOf(that)
    case that: JWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _                   => false
  }

  def isSubtypeOf (that: JObjectType): Boolean = {
    (this.erase == that.erase && matchTypeArgs(that.env)) || superType.exists(_.isSubtypeOf(that)) || interfaceTypes.exists(_.isSubtypeOf(that))
  }

  override def isAssignableTo(that: JType): Boolean = ???

  def matches (that: MetaValue): Boolean = this == that

  /* helper methods for collecting non-private inherited members */

  private def nonPrivateFieldList: List[JField] = {
    interfaceTypes.map(_.nonPrivateFieldList).reduceLeftOption(_ ++ _).getOrElse(Nil) ++
      superType.map(_.nonPrivateFieldList).getOrElse(Nil) ++
      declaredFields.filterNot(_.isPrivate)
  }

  private def nonPrivateMethodList: List[JMethod] = {
    interfaceTypes.map(_.nonPrivateMethodList).reduceLeftOption(_ ++ _).getOrElse(Nil) ++
      superType.map(_.nonPrivateMethodList).getOrElse(Nil) ++
      declaredMethods.filterNot(_.isPrivate)
  }

  private def filterOutOverriddenMethod (list: List[JMethod]): List[JMethod] = {
    list.foldRight[List[JMethod]](Nil) { (m, ms) =>
      if (ms.exists(_.overrides(m))) ms
      else m :: ms
    }
  }

  private def matchTypeArgs (args: Map[String, MetaValue]): Boolean = env.forall { case (key, value) =>
    args.get(key).exists { arg => arg.matches(value) }
  }
}

case class JPrimitiveType (clazz: JPrimitiveClass) extends JType {
  def name = clazz.name

  lazy val wrapperType: Option[JType] = clazz.wrapperClass.flatMap(_.objectType(Nil))

  def fields: Map[String, JField] = Map.empty
  def methods: Map[String, List[JMethod]] = Map.empty

  def isSubtypeOf (that: JType): Boolean = this == that

  def isAssignableTo(that: JType): Boolean = ???

  def compiler = clazz.compiler
}

case class JArrayType (componentType: JType) extends JRefType {
  def name: String = componentType.name + "[]"

  def superTypes: List[JObjectType] = compiler.typeLoader.superTypesOfArray

  def fields: Map[String, JField] = ???
  def methods: Map[String, List[JMethod]] = ???

  def isSubtypeOf (that: JType): Boolean = that match {
    case _ if this == that   => true
    case that: JArrayType    => componentType.isSubtypeOf(that.componentType)
    case that: JObjectType   => superTypes.exists(_.isSubtypeOf(that))
    case that: JWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _                   => false
  }

  def isAssignableTo (that: JType): Boolean = isSubtypeOf(that)

  def matches (that: MetaValue): Boolean = this == that

  def compiler: JCompiler = componentType.compiler
}

class JWildcardType (val upperBound: JType, val lowerBound: Option[JType], val compiler: JCompiler) extends JRefType {
  def name = lowerBound match {
    case Some(lb) => "? super " + lb.name
    case None     => "? extends " + upperBound.name
  }

  def methods: Map[String, List[JMethod]] = ???
  def fields: Map[String, JField] = ???


  def isSubtypeOf (that: JType): Boolean = upperBound.isSubtypeOf(that)
  def isAssignableTo (that: JType): Boolean = isSubtypeOf(that)

  def matches (that: MetaValue): Boolean = that match {
    case that: JRefType => that.isSubtypeOf(upperBound) && lowerBound.forall(_.isSubtypeOf(that))
    case _ => false
  }
}
