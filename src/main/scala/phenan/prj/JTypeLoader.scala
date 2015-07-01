package phenan.prj

import scalaz.Memo._

class JTypeLoader (compiler: JCompiler) {
  val arrayOf: JType => JArrayType = mutableHashMapMemo(getArrayType)

  def getObjectType (clazz: JClass, args: List[MetaValue]): Option[JObjectType] = {
    val result = getLoadedObjectType(clazz, args)
    if (validTypeArgs(clazz.signature.metaParams, args, result.env)) Some(result)
    else {
      state.error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
      None
    }
  }

  lazy val typeType = compiler.classLoader.typeClass.objectType(Nil)
  lazy val objectType = compiler.classLoader.objectClass.flatMap(_.objectType(Nil))
  lazy val stringType = compiler.classLoader.stringClass.flatMap(_.objectType(Nil))
  lazy val anyClassType = compiler.classLoader.classClass.flatMap(_.objectType(List(JWildcard(None, None))))

  lazy val boolean = compiler.classLoader.boolean.primitiveType
  lazy val void = compiler.classLoader.void.primitiveType

  lazy val superTypesOfArray: List[JObjectType] = CommonNames.superClassesOfArray.flatMap { name =>
    compiler.classLoader.loadClass_PE(name).flatMap(_.objectType(Nil))
  }

  def iterableOf (arg: JRefType) = compiler.classLoader.iterableClass.flatMap(_.objectType(List(arg)))

  def fromTypeSignature (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JType] = sig match {
    case p: JPrimitiveTypeSignature => Some(fromPrimitiveSignature(p))
    case s                          => fromTypeSignature_RefType(s, env)
  }

  def fromTypeSignature_RefType (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JRefType] = sig match {
    case cts: JClassTypeSignature        => fromClassTypeSignature(cts, env)
    case tvs: JTypeVariableSignature     => fromTypeVariableSignature(tvs, env)
    case JArrayTypeSignature(component)  => fromTypeSignature(component, env).map(_.array)
    case cap: JCapturedWildcardSignature => fromCapturedWildcardSignature(cap, env)
    case prm: JPrimitiveTypeSignature    =>
      state.error("do not use this method for primitive type signature : " + prm)
      None
  }

  def fromClassTypeSignature (sig: JClassTypeSignature, env: Map[String, MetaValue]): Option[JObjectType] = sig match {
    case JTypeSignature.typeTypeSig => typeType
    case SimpleClassTypeSignature(className, typeArgs) => for {
      clazz <- compiler.classLoader.loadClass_PE(className)
      args  <- fromTypeArguments(typeArgs, env)
    } yield getLoadedObjectType(clazz, args)
    case MemberClassTypeSignature(outer, name, typeArgs) => ???    // not supported yet
  }

  def fromTypeVariableSignature (sig: JTypeVariableSignature, env: Map[String, MetaValue]): Option[JRefType] = env.get(sig.name).flatMap {
    case t: JRefType  => Some(t)
    case w: JWildcard => w.upperBound.orElse(objectType).map(ub => JCapturedWildcardType(ub, w.lowerBound))
    case p: PureValue =>
      state.error("invalid type variable : " + sig.name)
      None
  }

  def fromCapturedWildcardSignature (sig: JCapturedWildcardSignature, env: Map[String, MetaValue]): Option[JCapturedWildcardType] = {
    sig.upperBound.flatMap(ub => fromTypeSignature_RefType(ub, env)).orElse(objectType).map { ub =>
      JCapturedWildcardType(ub, sig.lowerBound.flatMap(lb => fromTypeSignature_RefType(lb, env)))
    }
  }

  def fromPrimitiveSignature (p: JPrimitiveTypeSignature): JPrimitiveType = compiler.classLoader.erase(p).primitiveType

  def fromTypeArguments (args: List[JTypeArgument], env: Map[String, MetaValue]): Option[List[MetaValue]] = {
    import scalaz.Scalaz._
    args.traverse {
      case sig: JTypeSignature            => fromTypeSignature_RefType(sig, env)
      case WildcardArgument(upper, lower) => Some(JWildcard(upper.flatMap(fromTypeSignature_RefType(_, env)).filterNot(objectType.contains), lower.flatMap(fromTypeSignature_RefType(_, env))))
      case PureVariableSignature(name)    => env.get(name)
    }
  }

  def validTypeArgs (params: List[FormalMetaParameter], args: List[MetaValue], env: Map[String, MetaValue]): Boolean = {
    if (params.isEmpty || args.isEmpty) params.isEmpty && args.isEmpty
    else if (validTypeArg(params.head, args.head, env)) validTypeArgs(params.tail, args.tail, env)
    else false
  }

  private def validTypeArg (param: FormalMetaParameter, arg: MetaValue, env: Map[String, MetaValue]): Boolean = arg match {
    case arg: JRefType => param.bounds.forall(withinBound(_, arg, env))
    case pv: PureValue => fromTypeSignature(param.metaType, env).exists(pv.valueType <:< _)
    case wc: JWildcard => param.bounds.forall(bound => wc.upperBound.orElse(objectType).exists(upper => withinBound(bound, upper, env)))
  }

  private def withinBound (bound: JTypeSignature, arg: JRefType, env: Map[String, MetaValue]): Boolean = {
    arg.isSubtypeOf(fromTypeSignature(bound, env).get)
  }

  def state = compiler.state

  private def getArrayType (component: JType): JArrayType = JArrayType(component)

  private def getLoadedObjectType (clazz: JClass, args: List[MetaValue]): JObjectType = memoizedGetObjectType((clazz, clazz.signature.metaParams.map(_.name).zip(args).toMap))

  private val memoizedGetObjectType: ((JClass, Map[String, MetaValue])) => JObjectType = mutableHashMapMemo { pair => new JObjectType(pair._1, pair._2) }
}
