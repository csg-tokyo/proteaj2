package phenan.prj

import scalaz.Memo._
import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait JTypeLoader {
  this: JModules with JErasedTypes with JClassLoader with Application =>

  val arrayTypeOf: JType => JArrayType = mutableHashMapMemo(getArrayType)

  def getObjectType (clazz: JClass, args: List[MetaArgument]): Option[JObjectType] = {
    val result = getLoadedObjectType(clazz, args)
    if (validTypeArgs(clazz.signature.metaParams, args, result.env)) Some(result)
    else {
      error("invalid type arguments of class " + clazz.name + " : " + args.mkString("<", ",", ">"))
      None
    }
  }

  lazy val typeType: Option[JObjectType]     = getObjectType(typeClass, Nil)
  lazy val objectType: Option[JObjectType]   = getObjectType(objectClass, Nil)
  lazy val stringType: Option[JObjectType]   = getObjectType(stringClass, Nil)
  lazy val anyClassType: Option[JObjectType] = getObjectType(classClass, List(JWildcard(None, None)))

  lazy val byteType: JPrimitiveType    = byteClass.primitiveType
  lazy val charType: JPrimitiveType    = charClass.primitiveType
  lazy val doubleType: JPrimitiveType  = doubleClass.primitiveType
  lazy val floatType: JPrimitiveType   = floatClass.primitiveType
  lazy val intType: JPrimitiveType     = intClass.primitiveType
  lazy val longType: JPrimitiveType    = longClass.primitiveType
  lazy val shortType: JPrimitiveType   = shortClass.primitiveType
  lazy val booleanType: JPrimitiveType = booleanClass.primitiveType
  lazy val voidType: JPrimitiveType    = voidClass.primitiveType

  lazy val boxedByteType: Option[JObjectType]    = getObjectType(boxedByteClass, Nil)
  lazy val boxedCharType: Option[JObjectType]    = getObjectType(boxedCharClass, Nil)
  lazy val boxedDoubleType: Option[JObjectType]  = getObjectType(boxedDoubleClass, Nil)
  lazy val boxedFloatType: Option[JObjectType]   = getObjectType(boxedFloatClass, Nil)
  lazy val boxedIntType: Option[JObjectType]     = getObjectType(boxedIntClass, Nil)
  lazy val boxedLongType: Option[JObjectType]    = getObjectType(boxedLongClass, Nil)
  lazy val boxedShortType: Option[JObjectType]   = getObjectType(boxedShortClass, Nil)
  lazy val boxedBooleanType: Option[JObjectType] = getObjectType(boxedBooleanClass, Nil)
  lazy val boxedVoidType: Option[JObjectType]    = getObjectType(boxedVoidClass, Nil)

  lazy val boxedPrimitiveTypes: Map[JPrimitiveType, Option[JObjectType]] = Map(
    byteType -> boxedByteType, charType -> boxedCharType, doubleType -> boxedDoubleType,
    floatType -> boxedFloatType, intType -> boxedIntType, longType -> boxedLongType,
    shortType -> boxedShortType, booleanType -> boxedBooleanType, voidType -> boxedVoidType
  )

  lazy val superTypesOfArray: List[JObjectType] = CommonNames.superClassesOfArray.flatMap { name =>
    loadClass_NoFail(name).flatMap(getObjectType(_, Nil))
  }

  lazy val runtimeExceptionType: Option[JObjectType] = getObjectType(runtimeExceptionClass, Nil)
  lazy val errorType: Option[JObjectType] = getObjectType(errorClass, Nil)

  lazy val uncheckedExceptionTypes: List[JObjectType] = (runtimeExceptionType ++ errorType).toList

  def iterableOf (arg: JType): Option[JObjectType] = boxing(arg).flatMap(t => getObjectType(iterableClass, List(t)))
  def classTypeOf (arg: JType): Option[JObjectType] = boxing(arg).flatMap(t => getObjectType(classClass, List(t)))
  def functionTypeOf (from: JType, to: JType): Option[JObjectType] = for {
    f <- boxing(from)
    t <- boxing(to)
    r <- getObjectType(functionClass, List(f, t))
  } yield r

  def consumerTypeOf (arg: JType): Option[JObjectType] = boxing(arg).flatMap(t => getObjectType(consumerClass, List(t)))

  def boxing (t: JType): Option[JRefType] = t match {
    case ref: JRefType       => Some(ref)
    case prm: JPrimitiveType => boxedPrimitiveTypes(prm)
  }

  def fromTypeSignature (sig: JTypeSignature, env: Map[String, MetaArgument]): Option[JType] = sig match {
    case p: JPrimitiveTypeSignature => Some(fromPrimitiveSignature(p))
    case s                          => fromTypeSignature_RefType(s, env)
  }

  def fromTypeSignature_RefType (sig: JTypeSignature, env: Map[String, MetaArgument]): Option[JRefType] = sig match {
    case cts: JClassTypeSignature        => fromClassTypeSignature(cts, env)
    case tvs: JTypeVariableSignature     => fromTypeVariableSignature(tvs, env)
    case JArrayTypeSignature(component)  => fromTypeSignature(component, env).map(_.array)
    case cap: JCapturedWildcardSignature => fromCapturedWildcardSignature(cap, env)
    case prm: JPrimitiveTypeSignature    =>
      error("do not use this method for primitive type signature : " + prm)
      None
  }

  def fromClassTypeSignature (sig: JClassTypeSignature, env: Map[String, MetaArgument]): Option[JObjectType] = sig match {
    case JTypeSignature.typeTypeSig => typeType
    case SimpleClassTypeSignature(className, typeArgs) => for {
      clazz <- loadClass_NoFail(className)
      args  <- fromTypeArguments(typeArgs, env)
    } yield getLoadedObjectType(clazz, args)
    case MemberClassTypeSignature(_, _, _) => ???    // not supported yet
  }

  def fromTypeVariableSignature (sig: JTypeVariableSignature, env: Map[String, MetaArgument]): Option[JRefType] = env.get(sig.name).flatMap {
    case t: JRefType  => Some(t)
    case w: JWildcard => w.upperBound.orElse(objectType).map(ub => JCapturedWildcardType(ub, w.lowerBound))
    case _: MetaValue =>
      error("invalid type variable : " + sig.name)
      None
  }

  def fromCapturedWildcardSignature (sig: JCapturedWildcardSignature, env: Map[String, MetaArgument]): Option[JCapturedWildcardType] = {
    sig.upperBound.flatMap(ub => fromTypeSignature_RefType(ub, env)).orElse(objectType).map { ub =>
      JCapturedWildcardType(ub, sig.lowerBound.flatMap(lb => fromTypeSignature_RefType(lb, env)))
    }
  }

  def fromPrimitiveSignature (p: JPrimitiveTypeSignature): JPrimitiveType = erase(p).primitiveType

  def fromTypeArguments (args: List[JTypeArgument], env: Map[String, MetaArgument]): Option[List[MetaArgument]] = args.traverse {
    case sig: JTypeSignature            => fromTypeSignature_RefType(sig, env)
    case WildcardArgument(upper, lower) => Some(JWildcard(upper.flatMap(fromTypeSignature_RefType(_, env)).filterNot(objectType.contains), lower.flatMap(fromTypeSignature_RefType(_, env))))
    case MetaVariableSignature(name)    => env.get(name)
  }

  def validTypeArgs (params: List[FormalMetaParameter], args: List[MetaArgument], env: Map[String, MetaArgument]): Boolean = {
    if (params.isEmpty || args.isEmpty) params.isEmpty && args.isEmpty
    else if (validTypeArg(params.head, args.head, env)) validTypeArgs(params.tail, args.tail, env)
    else false
  }

  private def validTypeArg (param: FormalMetaParameter, arg: MetaArgument, env: Map[String, MetaArgument]): Boolean = arg match {
    case arg: JRefType => param.bounds.forall(withinBound(_, arg, env))
    case pv: MetaValue => fromTypeSignature(param.metaType, env).exists(pv.valueType <:< _)
    case wc: JWildcard => param.bounds.forall(bound => wc.upperBound.orElse(objectType).exists(upper => withinBound(bound, upper, env)))
  }

  private def withinBound (bound: JTypeSignature, arg: JRefType, env: Map[String, MetaArgument]): Boolean = {
    arg.isSubtypeOf(fromTypeSignature(bound, env).get)
  }

  private def getArrayType (component: JType): JArrayType = JArrayType(component)

  private def getLoadedObjectType (clazz: JClass, args: List[MetaArgument]): JObjectType = memoizedGetObjectType((clazz, clazz.signature.metaParams.map(_.name).zip(args).toMap))

  private val memoizedGetObjectType: ((JClass, Map[String, MetaArgument])) => JObjectType = mutableHashMapMemo { pair => JObjectType(pair._1, pair._2) }
}
