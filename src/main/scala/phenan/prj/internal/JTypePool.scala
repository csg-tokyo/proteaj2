package phenan.prj.internal

import phenan.prj._
import phenan.prj.exception._

import scala.util._

import scalaz.Memo._

object JTypePool {

  val arrayOf: JValueType => JArrayType = mutableHashMapMemo(getArrayType)
  val getClassType: JLoadedClass => JClassType = mutableHashMapMemo(getLoadedClassType)
  val getObjectType: (JLoadedClass, List[JValueType]) => Try[JObjectType] = Function.untupled(mutableHashMapMemo(getLoadedObjectType))

  private def fromClassTypeSignature (sig: ClassTypeSignature): Try[JObjectType] = {
    ???
  }

  private def getArrayType (component: JValueType): JArrayType = new JArrayTypeImpl(component)

  private def getLoadedClassType (clazz: JLoadedClass): JClassType = new JLoadedClassType(clazz)

  private def getLoadedObjectType (classAndArgs: (JLoadedClass, List[JValueType])): Try[JObjectType] = {
    val clazz = classAndArgs._1
    val args  = classAndArgs._2

    clazz.signature match {
      case Some(sig) =>
        val map = sig.typeParams.map(_.name).zip(args).toMap
        if (validTypeArgs(sig.typeParams, args, map, clazz.loader)) Success(new JLoadedObjectType(clazz, map))
        else Failure(InvalidTypeException("invalid type arguments of class " + clazz.name + " : " + args.map(_.name).mkString("<", ",", ">")))

      case None =>
        if (args.isEmpty) Success(new JLoadedObjectType(clazz, Map.empty))
        else Failure(InvalidTypeException("invalid type arguments of class " + clazz.name + " : " + args.map(_.name).mkString("<", ",", ">")))
    }
  }

  private def validTypeArgs (params: List[FormalTypeParameter], args: List[JValueType], env: Map[String, JValueType], loader: JClassLoader): Boolean = {
    if (params.isEmpty || args.isEmpty) params.isEmpty && args.isEmpty
    else if (validTypeArg(params.head, args.head, env, loader)) validTypeArgs(params.tail, args.tail, env, loader)
    else false
  }

  private def validTypeArg (param: FormalTypeParameter, arg: JValueType, env: Map[String, JValueType], loader: JClassLoader): Boolean = {
    param.classBound.forall(withinBound(_, arg, env, loader)) && param.interfaceBounds.forall(withinBound(_, arg, env, loader))
  }

  private def withinBound (bound: TypeSignature, arg: JValueType, env: Map[String, JValueType], loader: JClassLoader): Boolean = {
    arg.isSubtypeOf(typeSig2JType(bound, env, loader).get)
  }

  private def typeSig2JType (sig: TypeSignature, env: Map[String, JValueType], loader: JClassLoader): Try[JValueType] = sig match {
    case SimpleClassTypeSignature(className, typeArgs) => for {
      clazz <- loader.loadClass(className)
      args  <- typeArgs2JTypes(typeArgs, env, loader)
      jType <- clazz.objectType(args)
    } yield jType
    case MemberClassTypeSignature(outer, name, typeArgs) => ???    // not supported yet

    case TypeVariableSignature(name)   => env.get(name) match {
      case Some(t) => Success(t)
      case None    => Failure(InvalidTypeException("type parameter " + name + " is not bound"))
    }
    case ArrayTypeSignature(component) => typeSig2JType(component, env, loader).map(_.array)
    case p: PrimitiveTypeSignature     => Success(primSig2JPrimType(p, loader))
  }

  private def typeArgs2JTypes (args: List[TypeArgument], env: Map[String, JValueType], loader: JClassLoader): Try[List[JValueType]] = {
    import scalaz.Scalaz._
    import phenan.util._
    args.traverse(arg => argSig2JType(arg, env, loader))
  }

  private def argSig2JType (arg: TypeArgument, env: Map[String, JValueType], loader: JClassLoader): Try[JValueType] = arg match {
    case FixedTypeArgument(sig)          =>
      typeSig2JType(sig, env, loader)
    case UpperBoundWildcardArgument(sig) =>
      typeSig2JType(sig, env, loader).map(bound => new JWildcardTypeImpl(bound, None))
    case LowerBoundWildcardArgument(sig) => for {
      objectClass <- loader.loadClass("java/lang/Object")
      upperBound  <- objectClass.objectType(Nil)
      lowerBound  <- typeSig2JType(sig, env, loader)
    } yield new JWildcardTypeImpl(upperBound, Some(lowerBound))
    case UnboundWildcardArgument         => for {
      objectClass <- loader.loadClass("java/lang/Object")
      upperBound  <- objectClass.objectType(Nil)
    } yield new JWildcardTypeImpl(upperBound, None)
  }

  private def primSig2JPrimType (p: PrimitiveTypeSignature, loader: JClassLoader): JPrimitiveType = p match {
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
}
