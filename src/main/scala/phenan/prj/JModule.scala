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

class JWildcard (val upperBound: Option[JRefType], val lowerBound: Option[JRefType]) extends MetaValue {
  def name = upperBound.map(ub => "? extends " + ub.name).orElse(lowerBound.map(lb => "? super " + lb.name)).getOrElse("?")

  def matches (that: MetaValue): Boolean = that match {
    case that: JRefType  => upperBound.forall(that <:< _) && lowerBound.forall(_ <:< that)
    case that: JWildcard => upperBound.forall(ub => that.upperBound.exists(_ <:< ub)) && lowerBound.forall(lb => that.lowerBound.exists(lb <:< _))
    case _: PureValue => false
  }
}

case class JGenericType (signature: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler) {
  def bind (args: Map[String, MetaValue]): Option[JType] = {
    compiler.typeLoader.fromTypeSignature(signature, env ++ args)
  }
}

object JGenericType {/*
  def apply (metaParams: List[FormalMetaParameter], signature: JClassTypeSignature, args: List[JTypeArgument], env: Map[String, MetaValue]): JGenericType = signature match {
    case SimpleClassTypeSignature(clazz, as) =>
      SimpleClassTypeSignature(clazz, )
    ???
  }*/

  private def assignTypeArgument (a: JTypeArgument, map: Map[FormalMetaParameter, JTypeArgument]): Option[JTypeArgument] = a match {
    case sig: JTypeSignature => assignTypeSignature(sig, map)
    case PureVariable(name)  => map.find(_._1.name == name).map(_._2)
    case UpperBoundWildcardArgument(sig) => assignTypeSignature(sig, map).map(UpperBoundWildcardArgument)
    case LowerBoundWildcardArgument(sig) => assignTypeSignature(sig, map).map(LowerBoundWildcardArgument)
    case UnboundWildcardArgument => Some(UnboundWildcardArgument)
  }

  import scalaz.Scalaz._

  private def assignTypeSignature (sig: JTypeSignature, map: Map[FormalMetaParameter, JTypeArgument]): Option[JTypeSignature] = sig match {
    case prm: JPrimitiveTypeSignature => Some(prm)
    case JArrayTypeSignature(c)       => assignTypeSignature(c, map).map(JArrayTypeSignature)
    case JTypeVariableSignature(name) => map.find(_._1.name == name).map(_._2).flatMap {
      case sig: JTypeSignature               => Some(sig)
      case _: PureVariable                   => None
      case UpperBoundWildcardArgument(bound) => Some(JCapturedWildcardSignature(Some(bound), None))
      case LowerBoundWildcardArgument(bound) => Some(JCapturedWildcardSignature(None, Some(bound)))
      case UnboundWildcardArgument           => Some(JCapturedWildcardSignature(None, None))
    }
    case SimpleClassTypeSignature(clazz, args) =>
      args.traverse[Option, JTypeArgument](assignTypeArgument(_, map)).map(SimpleClassTypeSignature(clazz, _))
    case cap: JCapturedWildcardSignature       => Some(cap)
    case MemberClassTypeSignature(outer, clazz, args) => ???
  }
}

sealed trait JModule {
  def fields: Map[String, JField]
  def methods: Map[String, List[JMethod]]

  def compiler: JCompiler
}


case class JClassModule (clazz: JClass) extends JModule {
  lazy val declaredFields: List[JField] = clazz.fields.filter(_.isStatic).flatMap { fieldDef =>
    compiler.typeLoader.fromTypeSignature(fieldDef.signature, Map.empty).map(fieldType => new JField(fieldDef, fieldType, this))
  }

  lazy val fields: Map[String, JField] = declaredFields.filterNot(_.isPrivate).map(f => f.name -> f).toMap
  lazy val privateFields: Map[String, JField] = declaredFields.filter(_.isPrivate).map(f => f.name -> f).toMap

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

  def <=< (t: JGenericType): Option[Map[String, MetaValue]]
}

sealed trait JRefType extends JType with MetaValue

case class JObjectType (erase: JClass, env: Map[String, MetaValue]) extends JRefType {
  def compiler = erase.compiler

  def name: String = {
    if (env.isEmpty) erase.name
    else erase.name + env.map(kv => kv._1 + "=" + kv._2).mkString("<", ",", ">")
  }

  lazy val superType: Option[JObjectType] = compiler.typeLoader.fromClassTypeSignature(erase.signature.superClass, env)

  lazy val interfaceTypes: List[JObjectType] = erase.signature.interfaces.flatMap(compiler.typeLoader.fromClassTypeSignature(_, env))

  lazy val constructors: List[JConstructor] = {
    erase.methods.filter(_.isConstructor).map { constructorDef => new JConstructor(constructorDef, env, this) }
  }

  lazy val declaredFields: List[JField] = erase.fields.filterNot(_.isStatic).flatMap { fieldDef =>
    compiler.typeLoader.fromTypeSignature(fieldDef.signature, env).map(fieldType => new JField(fieldDef, fieldType, this))
  }

  lazy val declaredMethods: List[JMethod] = {
    erase.methods.filter(_.isInstanceMethod).map { methodDef => new JMethod(methodDef, env, this, erase) }
  }

  lazy val privateFields: Map[String, JField] = declaredFields.filter(_.isPrivate).map(f => f.name -> f).toMap
  lazy val privateMethods: Map[String, List[JMethod]] = declaredMethods.filter(_.isPrivate).groupBy(_.name)

  lazy val fields: Map[String, JField] = nonPrivateFieldList.map(f => f.name -> f).toMap
  lazy val methods: Map[String, List[JMethod]] = nonPrivateMethodList.groupBy(_.name).mapValues(filterOutOverriddenMethod)

  def isSubtypeOf (that: JType): Boolean = that match {
    case _ if this == that   => true
    case that: JObjectType   => isSubtypeOf(that)
    case that: JCapturedWildcardType       => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _: JPrimitiveType | _: JArrayType => false
  }

  def isSubtypeOf (that: JObjectType): Boolean = {
    (this.erase == that.erase && matchTypeArgs(that.env)) || superType.exists(_.isSubtypeOf(that)) || interfaceTypes.exists(_.isSubtypeOf(that))
  }

  def isAssignableTo(that: JType): Boolean = ???

  def <=< (t: JGenericType): Option[Map[String, MetaValue]] = t.signature match {
    case SimpleClassTypeSignature(clazz, args) if clazz == erase.name => ???
    case SimpleClassTypeSignature(clazz, args) => ???
/*      val x = compiler.classLoader.loadClass_PE(clazz).map { c =>
        val supers = c.signature.superClass :: c.signature.interfaces

      }
*/
    case JArrayTypeSignature(_) if compiler.typeLoader.superTypesOfArray.contains(this) => Some(t.env)
    case JTypeVariableSignature(name) if t.env.contains(name) => this <=< t.env(name)
    case _ => None
  }

  def <=< (m: MetaValue): Option[Map[String, MetaValue]] = ???

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

  override def <=<(t: JGenericType): Option[Map[String, MetaValue]] = ???

  def compiler = clazz.compiler
}

case class JArrayType (componentType: JType) extends JRefType {
  def name: String = componentType.name + "[]"

  def superTypes: List[JObjectType] = compiler.typeLoader.superTypesOfArray

  def fields: Map[String, JField] = ???
  def methods: Map[String, List[JMethod]] = ???

  def isSubtypeOf (that: JType): Boolean = that match {
    case _ if this == that => true
    case _: JPrimitiveType => false
    case that: JArrayType  => componentType.isSubtypeOf(that.componentType)
    case that: JObjectType => superTypes.exists(_.isSubtypeOf(that))
    case that: JCapturedWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
  }

  def isAssignableTo (that: JType): Boolean = isSubtypeOf(that)

  override def <=<(t: JGenericType): Option[Map[String, MetaValue]] = ???

  def matches (that: MetaValue): Boolean = this == that

  def compiler: JCompiler = componentType.compiler
}

case class JCapturedWildcardType private (upperBound: JRefType, lowerBound: Option[JRefType], id: Int) extends JRefType {
  override def name: String = "capture#" + id

  override def <=<(t: JGenericType): Option[Map[String, MetaValue]] = ???

  override def isSubtypeOf(that: JType): Boolean = upperBound.isSubtypeOf(that)
  override def isAssignableTo(that: JType): Boolean = isSubtypeOf(that)

  override def matches(v: MetaValue): Boolean = this == v

  override def methods: Map[String, List[JMethod]] = upperBound.methods

  override def fields: Map[String, JField] = upperBound.fields

  def compiler = upperBound.compiler
}

object JCapturedWildcardType {
  def apply (upperBound: JRefType, lowerBound: Option[JRefType]): JCapturedWildcardType = {
    val cap = JCapturedWildcardType(upperBound, lowerBound, id)
    id += 1
    cap
  }
  private var id = 0
}
