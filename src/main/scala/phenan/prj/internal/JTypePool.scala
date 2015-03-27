package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

import scalaz.Memo._

class JTypePool private (state: JState) {
  val arrayOf: JType with JType_Internal => JArrayType = mutableHashMapMemo(getArrayType)
  val getClassModule: JLoadedClass => JClassModule = mutableHashMapMemo(getLoadedClassModule)
  val getObjectType: (JLoadedClass, List[MetaValue]) => Option[JObjectType] = Function.untupled(mutableHashMapMemo(getLoadedObjectType))

  val superTypesOfArray: JClassLoader => List[JObjectType] = mutableHashMapMemo(getSuperTypesOfArray)

  def toJValueType (t: JErasedType): Option[JType] = t match {
    case c : JClass          => c.objectType(Nil)
    case p : JPrimitiveClass => Some(p.primitiveType)
    case a : JArrayClass     => toJValueType(a.component).map(_.array)
  }

  def rawTypeArguments (typeParams: List[FormalMetaParameter], env: Map[String, MetaValue], loader: JClassLoader): Map[String, MetaValue] = {
    typeParams.foldLeft(env) { (e, param) =>
      getRawMetaValue(param, e, loader).map(t => e + (param.name -> t)).getOrElse {
        state.error("cannot get raw meta value for " + param.name)
        e
      }
    }
  }

  private def getRawMetaValue (param: FormalMetaParameter, env: Map[String, MetaValue], loader: JClassLoader): Option[MetaValue] = {
    if (param.metaType != TypeSignature.typeTypeSig) fromTypeSignature(param.metaType, env, loader).map(UnknownPureValue)
    else param.bounds.headOption.flatMap(fromTypeSignature_RefType(_, env, loader)).orElse(loader.objectClass.objectType(Nil))
  }

  def fromTypeSignature (sig: TypeSignature, env: Map[String, MetaValue], loader: JClassLoader): Option[JType] = sig match {
    case p: PrimitiveTypeSignature => Some(fromPrimitiveSignature(p, loader))
    case s                         => fromTypeSignature_RefType(s, env, loader)
  }

  def fromClassTypeSignature (sig: ClassTypeSignature, env: Map[String, MetaValue], loader: JClassLoader): Option[JObjectType] = sig match {
    case SimpleClassTypeSignature(className, typeArgs) => for {
      clazz <- loader.loadClassOption(className)
      args  <- fromTypeArguments(typeArgs, env, loader)
      jType <- clazz.objectType(args)
    } yield jType
    case MemberClassTypeSignature(outer, name, typeArgs) => ???    // not supported yet
  }

  def fromPrimitiveSignature (p: PrimitiveTypeSignature, loader: JClassLoader): JPrimitiveType = p match {
    case ByteTypeSignature   => loader.byte.primitiveType
    case CharTypeSignature   => loader.char.primitiveType
    case DoubleTypeSignature => loader.double.primitiveType
    case FloatTypeSignature  => loader.float.primitiveType
    case IntTypeSignature    => loader.int.primitiveType
    case LongTypeSignature   => loader.long.primitiveType
    case ShortTypeSignature  => loader.short.primitiveType
    case BoolTypeSignature   => loader.boolean.primitiveType
    case VoidTypeSignature   => loader.void.primitiveType
  }

  def fromTypeArguments (args: List[TypeArgument], env: Map[String, MetaValue], loader: JClassLoader): Option[List[MetaValue]] = {
    import scalaz.Scalaz._
    args.traverse(arg => argSig2JType(arg, env, loader))
  }

  private def fromTypeSignature_RefType (sig: TypeSignature, env: Map[String, MetaValue], loader: JClassLoader): Option[JRefType] = sig match {
    case cts: ClassTypeSignature       => fromClassTypeSignature(cts, env, loader)
    case tvs: TypeVariableSignature    => fromTypeVariableSignature(tvs, env)
    case ArrayTypeSignature(component) => fromTypeSignature(component, env, loader).map(_.array)
    case _ =>
      state.error("invalid type signature : " + sig)
      None
  }

  private def fromTypeVariableSignature (sig: TypeVariableSignature, env: Map[String, MetaValue]): Option[JRefType] = env.get(sig.name) match {
    case Some(t: JRefType) => Some(t)
    case _ =>
      state.error("invalid type variable : " + sig.name)
      None
  }

  private def getArrayType (component: JType with JType_Internal): JArrayType = new JArrayTypeImpl(component)

  private def getLoadedClassModule (clazz: JLoadedClass): JClassModule = new JLoadedClassModule(clazz)

  private def getLoadedObjectType (classAndArgs: (JLoadedClass, List[MetaValue])): Option[JObjectType] = {
    val clazz = classAndArgs._1
    val args  = classAndArgs._2

    clazz.signature match {
      case Some(sig) =>
        val map = sig.metaParams.map(_.name).zip(args).toMap
        if (validTypeArgs(sig.metaParams, args, map, clazz.loader)) Some(new JLoadedObjectType(clazz, map))
        else {
          state.error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
          None
        }
      case None =>
        if (args.isEmpty) Some(new JLoadedObjectType(clazz, Map.empty))
        else {
          state.error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
          None
        }
    }
  }

  private def getSuperTypesOfArray (loader: JClassLoader): List[JObjectType] = {
    List("java/lang/Object", "java/io/Serializable", "java/lang/Cloneable").flatMap { name =>
      loader.loadClassOption(name).flatMap(_.objectType(Nil))
    }
  }

  private def validTypeArgs (params: List[FormalMetaParameter], args: List[MetaValue], env: Map[String, MetaValue], loader: JClassLoader): Boolean = {
    if (params.isEmpty || args.isEmpty) params.isEmpty && args.isEmpty
    else if (validTypeArg(params.head, args.head, env, loader)) validTypeArgs(params.tail, args.tail, env, loader)
    else false
  }

  private def validTypeArg (param: FormalMetaParameter, arg: MetaValue, env: Map[String, MetaValue], loader: JClassLoader): Boolean = arg match {
    case arg: JRefType => param.bounds.forall(withinBound(_, arg, env, loader))
    case pv: PureValue => fromTypeSignature(param.metaType, env, loader).exists(pv.valueType <:< _)
  }

  private def withinBound (bound: TypeSignature, arg: JRefType, env: Map[String, MetaValue], loader: JClassLoader): Boolean = {
    arg.isSubtypeOf(fromTypeSignature(bound, env, loader).get)
  }

  private def argSig2JType (arg: TypeArgument, env: Map[String, MetaValue], loader: JClassLoader): Option[MetaValue] = arg match {
    case sig: TypeSignature              =>
      fromTypeSignature_RefType(sig, env, loader)
    case UpperBoundWildcardArgument(sig) =>
      fromTypeSignature(sig, env, loader).map(bound => new JWildcardTypeImpl(bound, None, loader))
    case LowerBoundWildcardArgument(sig) => for {
      upperBound  <- loader.objectClass.objectType(Nil)
      lowerBound  <- fromTypeSignature(sig, env, loader)
    } yield new JWildcardTypeImpl(upperBound, Some(lowerBound), loader)
    case UnboundWildcardArgument         => for {
      upperBound  <- loader.objectClass.objectType(Nil)
    } yield new JWildcardTypeImpl(upperBound, None, loader)
    case PureVariable(name)              =>
      env.get(name)
  }

  private implicit def st: JState = state
}

object JTypePool {
  def get (implicit state: JState): JTypePool = pools(state)
  private val pools: JState => JTypePool = mutableHashMapMemo(state => new JTypePool(state))
}


