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

case class JWildcard (upperBound: Option[JRefType], lowerBound: Option[JRefType]) extends MetaValue {
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

  lazy val declaredMethods: List[JMethod] = clazz.methods.filter(_.isStaticMethod).map { methodDef =>
    new JMethod(methodDef, Map.empty, this, clazz)
  }

  lazy val methods: Map[String, List[JMethod]] = declaredMethods.filterNot(_.isPrivate).groupBy(_.name)
  lazy val privateMethods: Map[String, List[JMethod]] = declaredMethods.filter(_.isPrivate).groupBy(_.name)

  def compiler = clazz.compiler
}

sealed trait JType extends JModule {
  def name: String
  def array: JArrayType = compiler.typeLoader.arrayOf(this)

  def isSubtypeOf (that: JType): Boolean
  def isAssignableTo (that: JType): Boolean

  def unifyG (t: JGenericType): Option[Map[String, MetaValue]]
  def unifyL (t: JGenericType): Option[Map[String, MetaValue]]

  def <:< (t: JType): Boolean = this.isSubtypeOf(t)
  def >:> (t: JType): Boolean = t.isSubtypeOf(this)

  def <=< (t: JGenericType) = unifyG(t)
  def >=> (t: JGenericType) = unifyL(t)
}

sealed trait JRefType extends JType with MetaValue

case class JObjectType (erase: JClass, env: Map[String, MetaValue]) extends JRefType {
  def compiler = erase.compiler

  def name: String = {
    if (env.isEmpty) erase.name
    else erase.name + env.map(kv => kv._1 + "=" + kv._2).mkString("<", ",", ">")
  }

  def superType: Option[JObjectType] = compiler.typeLoader.fromClassTypeSignature(erase.signature.superClass, env)

  def interfaceTypes: List[JObjectType] = erase.signature.interfaces.flatMap(compiler.typeLoader.fromClassTypeSignature(_, env))

  lazy val superTypes: List[JObjectType] = superType match {
    case Some(sup) if sup != this => sup :: interfaceTypes
    case _ => interfaceTypes
  }

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
    (this.erase == that.erase && matchTypeArgs(that.env)) || superTypes.exists(_.isSubtypeOf(that))
  }

  def isAssignableTo(that: JType): Boolean = ???

  // List<String>.unifyG(ArrayList<T>) = Some(Map(T -> String))
  def unifyG (t: JGenericType): Option[Map[String, MetaValue]] = t.signature match {
    case cls : JClassTypeSignature        => unifyG(cls, t.env)
    case arr : JArrayTypeSignature        => unifyG(arr, t.env)
    case wil : JCapturedWildcardSignature => unifyG(wil, t.env)
    case tvr : JTypeVariableSignature     => JTypeUnification.unifyG(this, tvr, t.env)
    case prm : JPrimitiveTypeSignature    => None
  }

  // List<String>.unifyL(Collection<T>) = Some(Map(T -> String))
  def unifyL (t: JGenericType): Option[Map[String, MetaValue]] = t.signature match {
    case cls : JClassTypeSignature        => ???
    case arr : JArrayTypeSignature        => ???
    case tvr : JTypeVariableSignature     => ???
    case wil : JCapturedWildcardSignature => ???
    case prm : JPrimitiveTypeSignature    => ???
  }

  def matches (that: MetaValue): Boolean = this == that

  /* unification methods */

  private def unifyG (sig: JClassTypeSignature, e: Map[String, MetaValue]): Option[Map[String, MetaValue]] = unifyClassG(List(sig), Set.empty, e).flatMap {
    case SimpleClassTypeSignature(clazz, args) if erase.signature.metaParams.size == args.size => unifyArgsG(erase.signature.metaParams.zip(args), e)
    case _ => None
  }

  private def unifyG (sig: JArrayTypeSignature, e: Map[String, MetaValue]): Option[Map[String, MetaValue]] = {
    if (compiler.typeLoader.superTypesOfArray.contains(this)) Some(e)
    else None
  }

  private def unifyG (sig: JCapturedWildcardSignature, e: Map[String, MetaValue]): Option[Map[String, MetaValue]] = {
    unifyG(JGenericType(sig.upperBound.getOrElse(JTypeSignature.objectTypeSig), e, compiler))
  }

  private def unifyClassG (sigs: List[JClassTypeSignature], checked: Set[JClassTypeSignature], e: Map[String, MetaValue]): Option[JClassTypeSignature] = sigs match {
    case sig :: rest => sig match {
      case SimpleClassTypeSignature(clazz, _) if clazz == erase.internalName => Some(sig)
      case SimpleClassTypeSignature(clazz, args) => compiler.classLoader.loadClass_PE(clazz) match {
        case Some(cls) =>
          val cs = checked + sig
          val supers = directSuperTypes(cls, args, e).filterNot(cs.contains)
          unifyClassG(rest ++ supers, cs, e)
        case None => None
      }
      case MemberClassTypeSignature(_, clazz, _) if clazz == erase.internalName => Some(sig)
      case MemberClassTypeSignature(outer, clazz, args) => ???
    }
    case Nil => None
  }


  private def unifyArgsG (args: List[(FormalMetaParameter, JTypeArgument)], env: Map[String, MetaValue]): Option[Map[String, MetaValue]] = args match {
    case (param, arg) :: rest => this.env.get(param.name).flatMap(JTypeUnification.unifyG(_, arg, env, compiler)) match {
      case Some(e) => unifyArgsG(rest, e)
      case None => None
    }
    case Nil => Some(env)
  }

  /* helper methods for collecting non-private inherited members */

  private def nonPrivateFieldList: List[JField] = {
    superTypes.map(_.nonPrivateFieldList).reduceLeftOption(_ ++ _).getOrElse(Nil) ++ declaredFields.filterNot(_.isPrivate)
  }

  private def nonPrivateMethodList: List[JMethod] = {
    superTypes.map(_.nonPrivateMethodList).reduceLeftOption(_ ++ _).getOrElse(Nil) ++ declaredMethods.filterNot(_.isPrivate)
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

  /* */

  private def directSuperTypes (clazz: JClass, args: List[JTypeArgument], env: Map[String, MetaValue]): List[JClassTypeSignature] = {
    if (clazz.signature.metaParams.size == args.size && ! compiler.classLoader.objectClass.contains(clazz)) {
      val map = clazz.signature.metaParams.zip(args).toMap
      (clazz.signature.superClass :: clazz.signature.interfaces).flatMap(assignClassTypeSignature(_, map))
    }
    else Nil
  }

  private def assignTypeArgument (a: JTypeArgument, map: Map[FormalMetaParameter, JTypeArgument]): Option[JTypeArgument] = a match {
    case sig: JTypeSignature => assignTypeSignature(sig, map)
    case PureVariable(name)  => map.find(_._1.name == name).map(_._2)
    case UpperBoundWildcardArgument(sig) => assignTypeSignature(sig, map).map(UpperBoundWildcardArgument)
    case LowerBoundWildcardArgument(sig) => assignTypeSignature(sig, map).map(LowerBoundWildcardArgument)
    case UnboundWildcardArgument => Some(UnboundWildcardArgument)
  }

  private def assignTypeSignature (sig: JTypeSignature, map: Map[FormalMetaParameter, JTypeArgument]): Option[JTypeSignature] = sig match {
    case prm: JPrimitiveTypeSignature => Some(prm)
    case cts: JClassTypeSignature     => assignClassTypeSignature(cts, map)
    case JArrayTypeSignature(c)       => assignTypeSignature(c, map).map(JArrayTypeSignature)
    case JTypeVariableSignature(name) => map.find(_._1.name == name).map(_._2).flatMap {
      case sig: JTypeSignature               => Some(sig)
      case _: PureVariable                   => None
      case UpperBoundWildcardArgument(bound) => Some(JCapturedWildcardSignature(Some(bound), None))
      case LowerBoundWildcardArgument(bound) => Some(JCapturedWildcardSignature(None, Some(bound)))
      case UnboundWildcardArgument           => Some(JCapturedWildcardSignature(None, None))
    }
    case cap: JCapturedWildcardSignature     => Some(cap)
  }

  import scalaz.Scalaz._

  private def assignClassTypeSignature (sig: JClassTypeSignature, map: Map[FormalMetaParameter, JTypeArgument]): Option[JClassTypeSignature] = sig match {
    case SimpleClassTypeSignature(clazz, args) =>
      args.traverse[Option, JTypeArgument](assignTypeArgument(_, map)).map(SimpleClassTypeSignature(clazz, _))
    case MemberClassTypeSignature(outer, clazz, args) =>
      assignClassTypeSignature(outer, map).flatMap(o => args.traverse[Option, JTypeArgument](assignTypeArgument(_, map)).map(MemberClassTypeSignature(o, clazz, _)))
  }
}

object JTypeUnification {
  // unifyG (String, T, e) = Some(e + (T -> String))
  def unifyG (mv: MetaValue, arg: JTypeArgument, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = arg match {
    case sig: JTypeSignature             => unifyG(mv, sig, env, compiler)
    case pvr: PureVariable               => unifyG(mv, pvr, env, compiler)
    case ubw: UpperBoundWildcardArgument => unifyG(mv, ubw, env, compiler)
    case lbw: LowerBoundWildcardArgument => unifyG(mv, lbw, env, compiler)
    case UnboundWildcardArgument         => mv match {
      case JWildcard(None, None) => Some(env)
      case _ => None
    }
  }

  def unifyG (mv: MetaValue, tv: JTypeVariableSignature, env: Map[String, MetaValue]): Option[Map[String, MetaValue]] = {
    if (env.contains(tv.name)) {
      if (env(tv.name) == mv) Some(env)
      else None
    }
    else Some(env + (tv.name -> mv))
  }

  def unifyE (prm: JPrimitiveType, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = sig match {
    case _: JClassTypeSignature | _: JArrayTypeSignature | _: JTypeVariableSignature | _: JCapturedWildcardSignature => None
    case ByteTypeSignature if prm.clazz == compiler.classLoader.byte     => Some(env)
    case CharTypeSignature if prm.clazz == compiler.classLoader.char     => Some(env)
    case DoubleTypeSignature if prm.clazz == compiler.classLoader.double => Some(env)
    case FloatTypeSignature if prm.clazz == compiler.classLoader.float   => Some(env)
    case IntTypeSignature if prm.clazz == compiler.classLoader.int       => Some(env)
    case LongTypeSignature if prm.clazz == compiler.classLoader.long     => Some(env)
    case ShortTypeSignature if prm.clazz == compiler.classLoader.short   => Some(env)
    case BoolTypeSignature if prm.clazz == compiler.classLoader.boolean  => Some(env)
    case VoidTypeSignature if prm.clazz == compiler.classLoader.void     => Some(env)
  }

  private def unifyG (mv: MetaValue, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = mv match {
    case obj: JObjectType           => unifyG(obj, sig, env, compiler)
    case arr: JArrayType            => unifyG(arr, sig, env, compiler)
    case cap: JCapturedWildcardType => unifyG(cap, sig, env, compiler)
    case wil: JWildcard             => unifyG(wil, sig, env, compiler)
    case tvr: JTypeVariable         => unifyG(tvr, sig, env, compiler)
    case prv: PureValue             => None
  }

  private def unifyG (t: JType, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = t match {
    case obj: JObjectType           => unifyG(obj, sig, env, compiler)
    case arr: JArrayType            => unifyG(arr, sig, env, compiler)
    case cap: JCapturedWildcardType => unifyG(cap, sig, env, compiler)
    case tvr: JTypeVariable         => unifyG(tvr, sig, env, compiler)
    case prm: JPrimitiveType        => unifyE(prm, sig, env, compiler)
  }

  private def unifyG (mv: MetaValue, pvr: PureVariable, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = {
    if (env.contains(pvr.name)) {
      if (env(pvr.name) == mv) Some(env)
      else None
    }
    else Some(env + (pvr.name -> mv))
  }

  private def unifyG (mv: MetaValue, ub: UpperBoundWildcardArgument, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = mv match {
    case _: PureValue | _: JRefType | JWildcard(_, Some(_)) => None
    case JWildcard(None, None) => Some(env)
    case JWildcard(Some(u), None) => u.unifyG(JGenericType(ub.signature, env, compiler))
  }

  private def unifyG (mv: MetaValue, lb: LowerBoundWildcardArgument, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = mv match {
    case _: PureValue | _: JRefType | JWildcard(Some(_), _) => None
    case JWildcard(None, None) => Some(env)
    case JWildcard(None, Some(l)) => l.unifyL(JGenericType(lb.signature, env, compiler))
  }

  private def unifyG (obj: JObjectType, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = sig match {
    case SimpleClassTypeSignature(clazz, args) if obj.erase.internalName == clazz && obj.erase.signature.metaParams.size == args.size =>
      unifyArgsE(obj, obj.erase.signature.metaParams.zip(args), env, compiler)
    case MemberClassTypeSignature(outer, clazz, args) => ???
    case tv: JTypeVariableSignature => unifyG(obj, tv, env)
    case _: SimpleClassTypeSignature | _: JArrayTypeSignature | _: JCapturedWildcardSignature | _: JPrimitiveTypeSignature => None
  }

  private def unifyG (arr: JArrayType, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = sig match {
    case JArrayTypeSignature(component) => unifyG(arr.componentType, component, env, compiler)
    case tv: JTypeVariableSignature     => unifyG(arr, tv, env)
    case _: JClassTypeSignature | _: JPrimitiveTypeSignature | _: JCapturedWildcardSignature => None
  }

  private def unifyG (cap: JCapturedWildcardType, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = sig match {
    case tv: JTypeVariableSignature => unifyG(cap, tv, env)
    case _: JClassTypeSignature | _: JArrayTypeSignature | _: JCapturedWildcardSignature | _: JPrimitiveTypeSignature => None
  }

  private def unifyG (wild: JWildcard, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = sig match {
    case tv: JTypeVariableSignature => unifyG(wild, tv, env)
    case _: JPrimitiveTypeSignature => None
    case _ => wild.upperBound.orElse(compiler.typeLoader.objectType).flatMap(_.unifyG(JGenericType(sig, env, compiler)))
  }

  private def unifyG (tvr: JTypeVariable, sig: JTypeSignature, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = sig match {
    case wil : JCapturedWildcardSignature => wil.upperBound.flatMap(bound => unifyG(tvr, bound, env, compiler))
    case sig : JTypeVariableSignature     => unifyG(tvr, sig, env)
    case _: JClassTypeSignature | _: JArrayTypeSignature | _: JPrimitiveTypeSignature => None
  }

  private def unifyArgsE (obj: JObjectType, args: List[(FormalMetaParameter, JTypeArgument)], env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = args match {
    case (param, arg) :: rest => obj.env.get(param.name).flatMap(unifyE(_, arg, env, compiler)) match {
      case Some(e) => unifyArgsE(obj, rest, e, compiler)
      case None => None
    }
    case Nil => Some(env)
  }

  // TODO
  private def unifyE (mv: MetaValue, arg: JTypeArgument, env: Map[String, MetaValue], compiler: JCompiler): Option[Map[String, MetaValue]] = unifyG(mv, arg, env, compiler)
}

case class JPrimitiveType (clazz: JPrimitiveClass) extends JType {
  def name = clazz.name

  lazy val wrapperType: Option[JType] = clazz.wrapperClass.flatMap(_.objectType(Nil))

  def fields: Map[String, JField] = Map.empty
  def methods: Map[String, List[JMethod]] = Map.empty

  def isSubtypeOf (that: JType): Boolean = this == that

  def isAssignableTo(that: JType): Boolean = ???

  def unifyG (t: JGenericType): Option[Map[String, MetaValue]] = unifyE(t)
  def unifyL (t: JGenericType): Option[Map[String, MetaValue]] = unifyE(t)

  def unifyE (t: JGenericType): Option[Map[String, MetaValue]] = JTypeUnification.unifyE(this, t.signature, t.env, compiler)

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

  def unifyG (t: JGenericType): Option[Map[String, MetaValue]] = t.signature match {
    case JArrayTypeSignature(component) => componentType.unifyG(JGenericType(component, t.env, compiler))
    case tv: JTypeVariableSignature => JTypeUnification.unifyG(this, tv, t.env)
    case _: JClassTypeSignature | _: JPrimitiveTypeSignature | _: JCapturedWildcardSignature => None
  }

  def unifyL (t: JGenericType): Option[Map[String, MetaValue]] = ???

  def matches (that: MetaValue): Boolean = this == that

  def compiler: JCompiler = componentType.compiler
}

case class JTypeVariable (name: String, bounds: List[JRefType], compiler: JCompiler) extends JRefType {
  def isSubtypeOf(that: JType): Boolean = bounds.exists(_.isSubtypeOf(that)) | compiler.typeLoader.objectType.contains(that)
  def isAssignableTo(that: JType): Boolean = isSubtypeOf(that)

  override def unifyG (t: JGenericType): Option[Map[String, MetaValue]] = JTypeUnification.unifyG(this, t.signature, t.env, compiler)
    /*t.signature match {
    case cls : JClassTypeSignature        => None
    case arr : JArrayTypeSignature        => None
    case wil : JCapturedWildcardSignature => wil.upperBound.flatMap(bound => unifyG(JGenericType(bound, t.env, compiler)))
    case tvr : JTypeVariableSignature     => JTypeUnification.unifyG(this, tvr, t.env)
    case prm : JPrimitiveTypeSignature    => None
  }*/

  override def unifyL(t: JGenericType): Option[Map[String, MetaValue]] = ???

  override def matches(v: MetaValue): Boolean = ???

  def methods = boundHead.map(_.methods).getOrElse(Map.empty)
  def fields = boundHead.map(_.fields).getOrElse(Map.empty)

  private lazy val boundHead = bounds.headOption.orElse(compiler.typeLoader.objectType)
}

case class JCapturedWildcardType private (upperBound: JRefType, lowerBound: Option[JRefType], id: Int) extends JRefType {
  override def name: String = "capture#" + id

  def unifyG (t: JGenericType): Option[Map[String, MetaValue]] = lowerBound.flatMap(_.unifyG(t))
  def unifyL (t: JGenericType): Option[Map[String, MetaValue]] = upperBound.unifyL(t)

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
