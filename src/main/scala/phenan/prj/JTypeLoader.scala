package phenan.prj

import phenan.prj.exception._

import scala.util._
import scalaz.Memo._
import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait JTypeLoader {
  this: JModules with JErasedTypes with JClassLoader with Application =>

  val arrayTypeOf: JType => JArrayType = mutableHashMapMemo(getArrayType)

  def getObjectType (clazz: JClass, args: List[MetaArgument]): Try[JObjectType] = {
    val result = getLoadedObjectType(clazz, args)
    if (validTypeArgs(clazz.signature.metaParams, args, result.env)) Success(result)
    else Failure(InvalidTypeException(s"invalid type arguments of class ${clazz.name} : ${args.mkString("<", ",", ">")}"))
  }

  def getObjectType_NoFail(clazz: JClass, args: List[MetaArgument]): Option[JObjectType] = {
    getObjectType(clazz, args) match {
      case Success(t) => Some(t)
      case Failure(e) =>
        error("fail to load type", e)
        None
    }
  }

  private def getObjectType_Unsafe (clazz: JClass, args: List[MetaArgument]): JObjectType = {
    getObjectType(clazz, args).fold(e => throw InitializationFailedException(e), identity)
  }

  lazy val typeType: JObjectType     = getObjectType_Unsafe(typeClass, Nil)
  lazy val objectType: JObjectType   = getObjectType_Unsafe(objectClass, Nil)
  lazy val stringType: JObjectType   = getObjectType_Unsafe(stringClass, Nil)
  // lazy val anyClassType: JObjectType = getObjectType_Unsafe(classClass, List(JWildcard(None, None)))

  lazy val byteType: JPrimitiveType    = byteClass.primitiveType
  lazy val charType: JPrimitiveType    = charClass.primitiveType
  lazy val doubleType: JPrimitiveType  = doubleClass.primitiveType
  lazy val floatType: JPrimitiveType   = floatClass.primitiveType
  lazy val intType: JPrimitiveType     = intClass.primitiveType
  lazy val longType: JPrimitiveType    = longClass.primitiveType
  lazy val shortType: JPrimitiveType   = shortClass.primitiveType
  lazy val booleanType: JPrimitiveType = booleanClass.primitiveType
  lazy val voidType: JPrimitiveType    = voidClass.primitiveType

  lazy val boxedByteType: JObjectType    = getObjectType_Unsafe(boxedByteClass, Nil)
  lazy val boxedCharType: JObjectType    = getObjectType_Unsafe(boxedCharClass, Nil)
  lazy val boxedDoubleType: JObjectType  = getObjectType_Unsafe(boxedDoubleClass, Nil)
  lazy val boxedFloatType: JObjectType   = getObjectType_Unsafe(boxedFloatClass, Nil)
  lazy val boxedIntType: JObjectType     = getObjectType_Unsafe(boxedIntClass, Nil)
  lazy val boxedLongType: JObjectType    = getObjectType_Unsafe(boxedLongClass, Nil)
  lazy val boxedShortType: JObjectType   = getObjectType_Unsafe(boxedShortClass, Nil)
  lazy val boxedBooleanType: JObjectType = getObjectType_Unsafe(boxedBooleanClass, Nil)
  lazy val boxedVoidType: JObjectType    = getObjectType_Unsafe(boxedVoidClass, Nil)

  lazy val boxedPrimitiveTypes: Map[JPrimitiveType, JObjectType] = Map(
    byteType -> boxedByteType, charType -> boxedCharType, doubleType -> boxedDoubleType,
    floatType -> boxedFloatType, intType -> boxedIntType, longType -> boxedLongType,
    shortType -> boxedShortType, booleanType -> boxedBooleanType, voidType -> boxedVoidType
  )

  lazy val superTypesOfArray: List[JObjectType] = CommonNames.superClassesOfArray.flatMap { name =>
    loadClass_NoFail(name).flatMap(getObjectType_NoFail(_, Nil))
  }

  lazy val runtimeExceptionType: JObjectType = getObjectType_Unsafe(runtimeExceptionClass, Nil)
  lazy val errorType: JObjectType = getObjectType_Unsafe(errorClass, Nil)

  lazy val uncheckedExceptionTypes: List[JObjectType] = List(runtimeExceptionType, errorType)

  def iterableOf (arg: JType): JObjectType = getObjectType_Unsafe(iterableClass, List(boxing(arg)))
  def classTypeOf (arg: JType): JObjectType = getObjectType_Unsafe(classClass, List(boxing(arg)))

  def functionTypeOf (from: JType, to: JType): JObjectType = getObjectType_Unsafe(functionClass, List(boxing(from), boxing(to)))
  def consumerTypeOf (arg: JType): JObjectType = getObjectType_Unsafe(consumerClass, List(boxing(arg)))

  def scopedContextTypeOf (arg: JType): JObjectType = getObjectType_Unsafe(scopedContextClass, List(boxing(arg)))

  def pairTypeOf (arg1: JType, arg2: JType): JObjectType = getObjectType_Unsafe(pairClass, List(boxing(arg1), boxing(arg2)))

  def boxing (t: JType): JRefType = t match {
    case ref: JRefType       => ref
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
    case cap: JCapturedWildcardSignature => Some(fromCapturedWildcardSignature(cap, env))
    case prm: JPrimitiveTypeSignature    =>
      error("do not use this method for primitive type signature : " + prm)
      None
  }

  def fromClassTypeSignature (sig: JClassTypeSignature, env: Map[String, MetaArgument]): Option[JObjectType] = sig match {
    case JTypeSignature.typeTypeSig => Some(typeType)
    case SimpleClassTypeSignature(className, typeArgs) => for {
      clazz <- loadClass_NoFail(className)
      args  <- fromTypeArguments(typeArgs, env)
    } yield getLoadedObjectType(clazz, args)
    case MemberClassTypeSignature(_, _, _) => ???    // not supported yet
  }

  def fromTypeVariableSignature (sig: JTypeVariableSignature, env: Map[String, MetaArgument]): Option[JRefType] = env.get(sig.name).flatMap {
    case t: JRefType  => Some(t)
    case w: JWildcard => Some(JCapturedWildcardType(w.upperBound.getOrElse(objectType), w.lowerBound))
    case _: MetaValue =>
      error("invalid type variable : " + sig.name)
      None
  }

  def fromCapturedWildcardSignature (sig: JCapturedWildcardSignature, env: Map[String, MetaArgument]): JCapturedWildcardType = {
    JCapturedWildcardType(
      sig.upperBound.flatMap(ub => fromTypeSignature_RefType(ub, env)).getOrElse(objectType),
      sig.lowerBound.flatMap(lb => fromTypeSignature_RefType(lb, env)))
  }

  def fromPrimitiveSignature (p: JPrimitiveTypeSignature): JPrimitiveType = erase(p).primitiveType

  def fromTypeArguments (args: List[JTypeArgument], env: Map[String, MetaArgument]): Option[List[MetaArgument]] = args.traverse {
    case sig: JTypeSignature            => fromTypeSignature_RefType(sig, env)
    case WildcardArgument(upper, lower) => Some(JWildcard(upper.flatMap(fromTypeSignature_RefType(_, env)).filterNot(_ == objectType), lower.flatMap(fromTypeSignature_RefType(_, env))))
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
    case wc: JWildcard => param.bounds.forall(bound => withinBound(bound, wc.upperBound.getOrElse(objectType), env))
  }

  private def withinBound (bound: JTypeSignature, arg: JRefType, env: Map[String, MetaArgument]): Boolean = {
    arg.isSubtypeOf(fromTypeSignature(bound, env).get)
  }

  private def getArrayType (component: JType): JArrayType = JArrayType(component)

  private def getLoadedObjectType (clazz: JClass, args: List[MetaArgument]): JObjectType = memoizedGetObjectType((clazz, clazz.signature.metaParams.map(_.name).zip(args).toMap))

  private val memoizedGetObjectType: ((JClass, Map[String, MetaArgument])) => JObjectType = mutableHashMapMemo { pair => JObjectType(pair._1, pair._2) }
}
