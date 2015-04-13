package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

import scalaz.Memo._

class JTypeLoaderImpl (val compiler: JCompiler)(implicit state: JState) extends JTypeLoader {
  val arrayOf: JType => JArrayType = mutableHashMapMemo(getArrayType)
  val getObjectType: (JClass, List[MetaValue]) => Option[JObjectType] = Function.untupled(mutableHashMapMemo(Function.tupled(getLoadedObjectType)))

  def toJType (t: JErasedType): Option[JType] = t match {
    case c : JClass          => c.objectType(Nil)
    case p : JPrimitiveClass => Some(p.primitiveType)
    case a : JArrayClass     => toJType(a.component).map(_.array)
  }

  def rawTypeArguments (typeParams: List[FormalMetaParameter], env: Map[String, MetaValue]): Map[String, MetaValue] = {
    typeParams.foldLeft(env) { (e, param) =>
      getRawMetaValue(param, e).map(t => e + (param.name -> t)).getOrElse {
        state.error("cannot get raw meta value for " + param.name)
        e
      }
    }
  }

  def fromTypeSignature (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JType] = sig match {
    case p: JPrimitiveTypeSignature => Some(fromPrimitiveSignature(p))
    case s                          => fromTypeSignature_RefType(s, env)
  }

  def fromClassTypeSignature (sig: JClassTypeSignature, env: Map[String, MetaValue]): Option[JObjectType] = sig match {
    case SimpleClassTypeSignature(className, typeArgs) => for {
      clazz <- classLoader.loadClass_PE(className)
      args  <- fromTypeArguments(typeArgs, env)
      jType <- clazz.objectType(args)
    } yield jType
    case MemberClassTypeSignature(outer, name, typeArgs) => ???    // not supported yet
  }

  def fromPrimitiveSignature (p: JPrimitiveTypeSignature): JPrimitiveType = classLoader.erase(p).primitiveType

  def fromTypeArguments (args: List[JTypeArgument], env: Map[String, MetaValue]): Option[List[MetaValue]] = {
    import scalaz.Scalaz._
    args.traverse(arg => argSig2JType(arg, env))
  }

  private def getRawMetaValue (param: FormalMetaParameter, env: Map[String, MetaValue]): Option[MetaValue] = {
    if (param.metaType != JTypeSignature.typeTypeSig) fromTypeSignature(param.metaType, env).map(UnknownPureValue)
    else param.bounds.headOption.flatMap(fromTypeSignature_RefType(_, env)).orElse(objectType)
  }

  private def fromTypeSignature_RefType (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JRefType] = sig match {
    case cts: JClassTypeSignature        => fromClassTypeSignature(cts, env)
    case tvs: JTypeVariableSignature     => fromTypeVariableSignature(tvs, env)
    case JArrayTypeSignature(component)  => fromTypeSignature(component, env).map(_.array)
    case cap: JCapturedWildcardSignature => fromCapturedWildcardSignature(cap, env)
    case prm: JPrimitiveTypeSignature    =>
      state.error("do not use this method for primitive type signature : " + prm)
      None
  }

  private def fromTypeVariableSignature (sig: JTypeVariableSignature, env: Map[String, MetaValue]): Option[JRefType] = env.get(sig.name).flatMap {
    case t: JRefType  => Some(t)
    case w: JWildcard => w.upperBound.orElse(objectType).map(ub => JCapturedWildcardType(ub, w.lowerBound))
    case p: PureValue =>
      state.error("invalid type variable : " + sig.name)
      None
  }

  private def fromCapturedWildcardSignature (sig: JCapturedWildcardSignature, env: Map[String, MetaValue]): Option[JCapturedWildcardType] = {
    sig.upperBound.flatMap(ub => fromTypeSignature_RefType(ub, env)).orElse(objectType).map { ub =>
      JCapturedWildcardType(ub, sig.lowerBound.flatMap(lb => fromTypeSignature_RefType(lb, env)))
    }
  }

  private def getArrayType (component: JType): JArrayType = JArrayType(component)

  private def getLoadedObjectType: (JClass, List[MetaValue]) => Option[JObjectType] = { case (clazz, args) =>
    val map = clazz.signature.metaParams.map(_.name).zip(args).toMap
    if (validTypeArgs(clazz.signature.metaParams, args, map)) Some(new JObjectType(clazz, map))
    else {
      state.error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
      None
    }
  }

  private def validTypeArgs (params: List[FormalMetaParameter], args: List[MetaValue], env: Map[String, MetaValue]): Boolean = {
    if (params.isEmpty || args.isEmpty) params.isEmpty && args.isEmpty
    else if (validTypeArg(params.head, args.head, env)) validTypeArgs(params.tail, args.tail, env)
    else false
  }

  private def validTypeArg (param: FormalMetaParameter, arg: MetaValue, env: Map[String, MetaValue]): Boolean = arg match {
    case arg: JRefType => param.bounds.forall(withinBound(_, arg, env))
    case pv: PureValue => fromTypeSignature(param.metaType, env).exists(pv.valueType <:< _)
    case wc: JWildcard => param.bounds.forall(bound => wc.lowerBound.exists(lower => withinBound(bound, lower, env)))
  }

  private def withinBound (bound: JTypeSignature, arg: JRefType, env: Map[String, MetaValue]): Boolean = {
    arg.isSubtypeOf(fromTypeSignature(bound, env).get)
  }

  private def argSig2JType (arg: JTypeArgument, env: Map[String, MetaValue]): Option[MetaValue] = arg match {
    case sig: JTypeSignature             => fromTypeSignature_RefType(sig, env)
    case UpperBoundWildcardArgument(sig) => fromTypeSignature_RefType(sig, env).map { bound =>
      if (objectType.contains(bound)) JWildcard(None, None)
      else JWildcard(Some(bound), None)
    }
    case LowerBoundWildcardArgument(sig) => fromTypeSignature_RefType(sig, env).map(lb => JWildcard(None, Some(lb)))
    case UnboundWildcardArgument         => Some(JWildcard(None, None))
    case PureVariable(name)              => env.get(name)
  }

  private def classLoader = compiler.classLoader
}
