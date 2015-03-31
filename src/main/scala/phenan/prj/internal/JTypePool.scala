package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

import scalaz.Memo._

class JTypePool (val compiler: JCompiler)(implicit state: JState) extends JTypeLoader {
  val arrayOf: JType => JArrayType = mutableHashMapMemo(getArrayType)
  val getObjectType: (JClass, List[MetaValue]) => Option[JObjectType] = Function.untupled(mutableHashMapMemo(getLoadedObjectType))

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

  def fromPrimitiveSignature (p: JPrimitiveTypeSignature): JPrimitiveType = p match {
    case ByteTypeSignature   => classLoader.byte.primitiveType
    case CharTypeSignature   => classLoader.char.primitiveType
    case DoubleTypeSignature => classLoader.double.primitiveType
    case FloatTypeSignature  => classLoader.float.primitiveType
    case IntTypeSignature    => classLoader.int.primitiveType
    case LongTypeSignature   => classLoader.long.primitiveType
    case ShortTypeSignature  => classLoader.short.primitiveType
    case BoolTypeSignature   => classLoader.boolean.primitiveType
    case VoidTypeSignature   => classLoader.void.primitiveType
  }

  def fromTypeArguments (args: List[JTypeArgument], env: Map[String, MetaValue]): Option[List[MetaValue]] = {
    import scalaz.Scalaz._
    args.traverse(arg => argSig2JType(arg, env))
  }

  def unifyCovariant (t1: JGenericType, t2: JType): Option[Map[String, MetaValue]] = (t1.signature, t2) match {
    case (cts: SimpleClassTypeSignature, obj: JObjectType) if classLoader.loadClass_PE(cts.clazz).contains(obj.erase) =>
      obj.erase.signature match {
        case Some(sig) => unifyCovariantTypeArguments(sig.metaParams, sig.metaParams.zip(cts.args), obj.env, t1.env)
        case None => Some(t1.env)
      }

    case _ => None
  }

  def unifyCovariantTypeArguments (formalParams: List[FormalMetaParameter], metaParams1: List[(FormalMetaParameter, JTypeArgument)], metaParams2: Map[String, MetaValue], env: Map[String, MetaValue]): Option[Map[String, MetaValue]] = {
    metaParams1 match {
      case (formalParam, typeArg) :: rest =>
        metaParams2.get(formalParam.name).flatMap(mv => unifyCovariantTypeArgument(typeArg, mv, env)) match {
          case Some(e) => unifyCovariantTypeArguments(formalParams, rest, metaParams2, e)
          case None => None
        }
      case Nil =>
        if (validTypeArgs(formalParams, formalParams.map(_.name).flatMap(env.get), env)) Some(env)
        else None
    }
  }

  def unifyCovariantTypeArgument (arg: JTypeArgument, mv: MetaValue, env: Map[String, MetaValue]): Option[Map[String, MetaValue]] = ???

/*
  def unifyCovariant (signature: TypeSignature, t: JType, env: Map[String, MetaValue], loader: JClassLoader): Option[Map[String, MetaValue]] = (signature, t) match {
    case (cts: SimpleClassTypeSignature, obj: JObjectType) => unifyClassTypeSignature(cts, obj, env, loader)
    case (prm: PrimitiveTypeSignature, pt: JPrimitiveType) if fromPrimitiveSignature(prm, loader) == pt => Some(env)
    case (arr: ArrayTypeSignature, at: JArrayType) =>
  }

  def unifyContravariant (signature: TypeSignature, t: JType, env: Map[String, MetaValue], loader: JClassLoader): Option[Map[String, MetaValue]] = (signature, t) match {
    case (cts: SimpleClassTypeSignature, obj: JObjectType) => unifyClassTypeSignature(cts, obj, env, loader)
  }

  private def unifyInvariant (signature: TypeSignature, t: JRefType, env: Map[String, MetaValue], loader: JClassLoader): Option[Map[String, MetaValue]] = {

  }

  private def unifyClassTypeSignature(signature: SimpleClassTypeSignature, t: JObjectType, env: Map[String, MetaValue], loader: JClassLoader): Option[Map[String, MetaValue]] = {
    if (loader.loadClassOption(signature.clazz).contains(t.erase)) unifyTypeArguments(t.erase.metaParameterNames.zip(signature.args), t.env, env, loader)
    // else if () 継承
    else None
  }

  private def unifyTypeArguments(args: List[(String, TypeArgument)], env: Map[String, MetaValue], enclosingEnv: Map[String, MetaValue], loader: JClassLoader): Option[Map[String, MetaValue]] = {
    args.foldLeft(Option(enclosingEnv)) {
      case (Some(e), (name, arg)) => env.get(name).flatMap(mv => unifyTypeArgument(arg, mv, e, loader))
    }
  }

  private def unifyTypeArgument(arg: TypeArgument, mv: MetaValue, env: Map[String, MetaValue], loader: JClassLoader): Option[Map[String, MetaValue]] = (arg, mv) match {
    case (sig: TypeSignature, t: JRefType)             => unifyInvariant(sig, t, env, loader)
    case (ub: UpperBoundWildcardArgument, t: JRefType) => Some(env)
    case (lb: LowerBoundWildcardArgument, t: JRefType) => Some(env)
    case (UnboundWildcardArgument, t: JRefType)        => Some(env)
    case (pv: PureVariable, v: PureValue)              => Some(env + (pv.name -> v))
    case _ => None
  }*/

  private def getRawMetaValue (param: FormalMetaParameter, env: Map[String, MetaValue]): Option[MetaValue] = {
    if (param.metaType != JTypeSignature.typeTypeSig) fromTypeSignature(param.metaType, env).map(UnknownPureValue)
    else param.bounds.headOption.flatMap(fromTypeSignature_RefType(_, env)).orElse(classLoader.objectClass.objectType(Nil))
  }

  private def fromTypeSignature_RefType (sig: JTypeSignature, env: Map[String, MetaValue]): Option[JRefType] = sig match {
    case cts: JClassTypeSignature       => fromClassTypeSignature(cts, env)
    case tvs: JTypeVariableSignature    => fromTypeVariableSignature(tvs, env)
    case JArrayTypeSignature(component) => fromTypeSignature(component, env).map(_.array)
    case _ =>
      state.error("invalid type signature : " + sig)
      None
  }

  private def fromTypeVariableSignature (sig: JTypeVariableSignature, env: Map[String, MetaValue]): Option[JRefType] = env.get(sig.name) match {
    case Some(t: JRefType) => Some(t)
    case _ =>
      state.error("invalid type variable : " + sig.name)
      None
  }

  private def getArrayType (component: JType): JArrayType = new JArrayTypeImpl(component)

  private def getLoadedObjectType (classAndArgs: (JClass, List[MetaValue])): Option[JObjectType] = {
    val clazz = classAndArgs._1
    val args  = classAndArgs._2

    clazz.signature match {
      case Some(sig) =>
        val map = sig.metaParams.map(_.name).zip(args).toMap
        if (validTypeArgs(sig.metaParams, args, map)) Some(new JObjectTypeImpl(clazz, map))
        else {
          state.error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
          None
        }
      case None =>
        if (args.isEmpty) Some(new JObjectTypeImpl(clazz, Map.empty))
        else {
          state.error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
          None
        }
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
  }

  private def withinBound (bound: JTypeSignature, arg: JRefType, env: Map[String, MetaValue]): Boolean = {
    arg.isSubtypeOf(fromTypeSignature(bound, env).get)
  }

  private def argSig2JType (arg: JTypeArgument, env: Map[String, MetaValue]): Option[MetaValue] = arg match {
    case sig: JTypeSignature              =>
      fromTypeSignature_RefType(sig, env)
    case UpperBoundWildcardArgument(sig) =>
      fromTypeSignature(sig, env).map(bound => new JWildcardTypeImpl(bound, None, compiler))
    case LowerBoundWildcardArgument(sig) => for {
      upperBound  <- classLoader.objectClass.objectType(Nil)
      lowerBound  <- fromTypeSignature(sig, env)
    } yield new JWildcardTypeImpl(upperBound, Some(lowerBound), compiler)
    case UnboundWildcardArgument         => for {
      upperBound  <- classLoader.objectClass.objectType(Nil)
    } yield new JWildcardTypeImpl(upperBound, None, compiler)
    case PureVariable(name)              =>
      env.get(name)
  }

  private def classLoader = compiler.classLoader
}
