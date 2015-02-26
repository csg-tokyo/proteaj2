package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

import scalaz.Memo._

class JTypePool private (state: JState) {
  val arrayOf: JValueType with JType_Internal => JArrayType = mutableHashMapMemo(getArrayType)
  val getClassType: JLoadedClass => JClassType = mutableHashMapMemo(getLoadedClassType)
  val getObjectType: (JLoadedClass, List[JValueType]) => Option[JObjectType] = Function.untupled(mutableHashMapMemo(getLoadedObjectType))

  val superTypesOfArray: JClassLoader => List[JObjectType] = mutableHashMapMemo(getSuperTypesOfArray)

  def toJValueType (t: JErasedType): Option[JValueType] = t match {
    case c : JClass          => c.objectType(Nil)
    case p : JPrimitiveClass => Some(p.primitiveType)
    case a : JArrayClass     => toJValueType(a.component).map(_.array)
  }

  def rawTypeArguments (typeParams: List[FormalTypeParameter], env: Map[String, JValueType], loader: JClassLoader): Map[String, JValueType] = {
    typeParams.foldLeft(env) { (e, param) =>
      param.classBound.orElse(param.interfaceBounds.headOption).flatMap(fromTypeSignature(_, e, loader)).
        orElse(loader.objectClass.flatMap(_.objectType(Nil))).map(t => e + (param.name -> t)).getOrElse {
        state.error("cannot load object type")
        e
      }
    }
  }

  def fromTypeSignature (sig: TypeSignature, env: Map[String, JValueType], loader: JClassLoader): Option[JValueType] = sig match {
    case cts: ClassTypeSignature       => fromClassTypeSignature(cts, env, loader)
    case TypeVariableSignature(name)   => env.get(name)
    case ArrayTypeSignature(component) => fromTypeSignature(component, env, loader).map(_.array)
    case p: PrimitiveTypeSignature     => Some(fromPrimitiveSignature(p, loader))
  }

  def fromClassTypeSignature (sig: ClassTypeSignature, env: Map[String, JValueType], loader: JClassLoader): Option[JObjectType] = sig match {
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

  def fromTypeArguments (args: List[TypeArgument], env: Map[String, JValueType], loader: JClassLoader): Option[List[JValueType]] = {
    import scalaz.Scalaz._
    args.traverse(arg => argSig2JType(arg, env, loader))
  }

  private def getArrayType (component: JValueType with JType_Internal): JArrayType = new JArrayTypeImpl(component)

  private def getLoadedClassType (clazz: JLoadedClass): JClassType = new JLoadedClassType(clazz)

  private def getLoadedObjectType (classAndArgs: (JLoadedClass, List[JValueType])): Option[JObjectType] = {
    val clazz = classAndArgs._1
    val args  = classAndArgs._2

    clazz.signature match {
      case Some(sig) =>
        val map = sig.typeParams.map(_.name).zip(args).toMap
        if (validTypeArgs(sig.typeParams, args, map, clazz.loader)) Some(new JLoadedObjectType(clazz, map))
        else {
          state.error("invalid type arguments of class " + clazz.name + " : " + args.map(_.name).mkString("<", ",", ">"))
          None
        }
      case None =>
        if (args.isEmpty) Some(new JLoadedObjectType(clazz, Map.empty))
        else {
          state.error("invalid type arguments of class " + clazz.name + " : " + args.map(_.name).mkString("<", ",", ">"))
          None
        }
    }
  }

  private def getSuperTypesOfArray (loader: JClassLoader): List[JObjectType] = {
    List("java/lang/Object", "java/io/Serializable", "java/lang/Cloneable").flatMap { name =>
      loader.loadClassOption(name).flatMap(_.objectType(Nil))
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
    arg.isSubtypeOf(fromTypeSignature(bound, env, loader).get)
  }

  private def argSig2JType (arg: TypeArgument, env: Map[String, JValueType], loader: JClassLoader): Option[JValueType] = arg match {
    case FixedTypeArgument(sig)          =>
      fromTypeSignature(sig, env, loader)
    case UpperBoundWildcardArgument(sig) =>
      fromTypeSignature(sig, env, loader).map(bound => new JWildcardTypeImpl(bound, None, loader))
    case LowerBoundWildcardArgument(sig) => for {
      objectClass <- loader.objectClass
      upperBound  <- objectClass.objectType(Nil)
      lowerBound  <- fromTypeSignature(sig, env, loader)
    } yield new JWildcardTypeImpl(upperBound, Some(lowerBound), loader)
    case UnboundWildcardArgument         => for {
      objectClass <- loader.objectClass
      upperBound  <- objectClass.objectType(Nil)
    } yield new JWildcardTypeImpl(upperBound, None, loader)
  }

  private implicit def st: JState = state
}

object JTypePool {
  def get (implicit state: JState): JTypePool = pools(state)
  private val pools: JState => JTypePool = mutableHashMapMemo(state => new JTypePool(state))
}


