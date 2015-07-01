package phenan.prj

sealed trait MetaValue {
  def matches (v: MetaValue): Boolean
}

sealed trait PureValue extends MetaValue {
  def valueType: JType
}

case class PureVariableRef (name: String, valueType: JType) extends PureValue {
  def matches (v: MetaValue): Boolean = this == v
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
  def unbound (args: Map[String, MetaValue]): Set[String] = unbound(signature, args, Set.empty[String])

  private def unbound (sig: JTypeSignature, args: Map[String, MetaValue], result: Set[String]): Set[String] = sig match {
    case JTypeVariableSignature(name) if args.contains(name) || env.contains(name) => result
    case JTypeVariableSignature(name)           => result + name
    case SimpleClassTypeSignature(_, as)        => as.foldLeft(result) { (r, a) => unbound(a, args, r) }
    case MemberClassTypeSignature(outer, _, as) => as.foldLeft(unbound(outer, args, result)) { (r, a) => unbound(a, args, r) }
    case JArrayTypeSignature(component)         => unbound(component, args, result)
    case JCapturedWildcardSignature(ub, lb)     => ub.map(unbound(_, args, result)).orElse(lb.map(unbound(_, args, result))).getOrElse(result)
    case _ : JPrimitiveTypeSignature            => result
  }

  private def unbound (sig: JTypeArgument, args: Map[String, MetaValue], result: Set[String]): Set[String] = sig match {
    case PureVariableSignature(name) if args.contains(name) || env.contains(name) => result
    case PureVariableSignature(name) => result + name
    case sig: JTypeSignature         => unbound(sig, args, result)
    case WildcardArgument(ub, lb)    => ub.map(unbound(_, args, result)).orElse(lb.map(unbound(_, args, result))).getOrElse(result)
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


  def priorities = clazz.priorities
  def constraints = clazz.priorityConstraints

  lazy val withDSLs = clazz.withDSLs.flatMap(compiler.classLoader.erase_PE).map(_.classModule)

  def compiler = clazz.compiler

  lazy val expressionOperators = collectExpressionOperators(declaredMethods.filterNot(_.isPrivate), Nil)
  lazy val literalOperators = collectLiteralOperators(declaredMethods.filterNot(_.isPrivate), Nil)

  private def collectExpressionOperators (ms: List[JMethod], es: List[(JExpressionSyntax, JMethod)]): List[(JExpressionSyntax, JMethod)] = ms match {
    case m :: rest => m.syntax match {
      case Some(s: JExpressionSyntax) => collectExpressionOperators(rest, (s -> m) :: es)
      case _ => collectExpressionOperators(rest, es)
    }
    case Nil => es
  }

  private def collectLiteralOperators (ms: List[JMethod], es: List[(JLiteralSyntax, JMethod)]): List[(JLiteralSyntax, JMethod)] = ms match {
    case m :: rest => m.syntax match {
      case Some(s: JLiteralSyntax) => collectLiteralOperators(rest, (s -> m) :: es)
      case _ => collectLiteralOperators(rest, es)
    }
    case Nil => es
  }
}

sealed trait JType extends JModule {
  def name: String
  def array: JArrayType = compiler.typeLoader.arrayOf(this)
  def array (dim: Int): JType = {
    if (dim > 0) array.array(dim - 1)
    else this
  }

  def boxed: Option[JRefType]

  def isSubtypeOf (that: JType): Boolean
  def isAssignableTo (that: JType): Boolean

  def unifyG (t: JGenericType): Option[Map[String, MetaValue]] = compiler.unifier.unify(this, t)
  def unifyL (t: JGenericType): Option[Map[String, MetaValue]] = compiler.unifier.infer(this, t)

  def <:< (t: JType): Boolean = this.isSubtypeOf(t)
  def >:> (t: JType): Boolean = t.isSubtypeOf(this)

  def <=< (t: JGenericType) = unifyG(t)
  def >=> (t: JGenericType) = unifyL(t)
}

sealed trait JRefType extends JType with MetaValue {
  def boxed = Some(this)
}

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
    case that: JCapturedWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _: JPrimitiveType | _: JArrayType | _: JUnboundTypeVariable => false
  }

  def isSubtypeOf (that: JObjectType): Boolean = {
    (this.erase == that.erase && matchTypeArgs(that.env)) || superTypes.exists(_.isSubtypeOf(that))
  }

  def isAssignableTo(that: JType): Boolean = ???

  def matches (that: MetaValue): Boolean = this == that

  lazy val expressionOperators = collectExpressionOperators(nonPrivateMethodList, Nil)
  lazy val literalOperators = collectLiteralOperators(nonPrivateMethodList, Nil)

  private def collectExpressionOperators (ms: List[JMethod], es: List[(JExpressionSyntax, JMethod)]): List[(JExpressionSyntax, JMethod)] = ms match {
    case m :: rest => m.syntax match {
      case Some(s: JExpressionSyntax) => collectExpressionOperators(rest, (s -> m) :: es)
      case _ => collectExpressionOperators(rest, es)
    }
    case Nil => es
  }

  private def collectLiteralOperators (ms: List[JMethod], es: List[(JLiteralSyntax, JMethod)]): List[(JLiteralSyntax, JMethod)] = ms match {
    case m :: rest => m.syntax match {
      case Some(s: JLiteralSyntax) => collectLiteralOperators(rest, (s -> m) :: es)
      case _ => collectLiteralOperators(rest, es)
    }
    case Nil => es
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
}

case class JPrimitiveType (clazz: JPrimitiveClass) extends JType {
  def name = clazz.name

  def fields: Map[String, JField] = Map.empty
  def methods: Map[String, List[JMethod]] = Map.empty

  def isSubtypeOf (that: JType): Boolean = this == that

  def isAssignableTo(that: JType): Boolean = ???

  lazy val boxed: Option[JRefType] = clazz.wrapperClass.flatMap(_.objectType(Nil))

  def compiler = clazz.compiler
}

case class JArrayType (componentType: JType) extends JRefType {
  def name: String = componentType.name + "[]"

  def superTypes: List[JObjectType] = compiler.typeLoader.superTypesOfArray

  def fields: Map[String, JField] = ???
  def methods: Map[String, List[JMethod]] = ???

  def isSubtypeOf (that: JType): Boolean = that match {
    case _ if this == that => true
    case that: JArrayType  => componentType.isSubtypeOf(that.componentType)
    case that: JObjectType => superTypes.exists(_.isSubtypeOf(that))
    case that: JCapturedWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
    case _: JPrimitiveType | _: JUnboundTypeVariable => false
  }

  def isAssignableTo (that: JType): Boolean = isSubtypeOf(that)

  def matches (that: MetaValue): Boolean = this == that

  def compiler: JCompiler = componentType.compiler
}

case class JTypeVariable (name: String, bounds: List[JRefType], compiler: JCompiler) extends JRefType {
  def isSubtypeOf(that: JType): Boolean = bounds.exists(_.isSubtypeOf(that)) | compiler.typeLoader.objectType.contains(that)
  def isAssignableTo(that: JType): Boolean = isSubtypeOf(that)

  override def matches (v: MetaValue): Boolean = this == v

  def methods = boundHead.map(_.methods).getOrElse(Map.empty)
  def fields = boundHead.map(_.fields).getOrElse(Map.empty)

  private lazy val boundHead = bounds.headOption.orElse(compiler.typeLoader.objectType)
}

case class JCapturedWildcardType private (upperBound: JRefType, lowerBound: Option[JRefType], id: Int) extends JRefType {
  override def name: String = "capture#" + id

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

case class JUnboundTypeVariable (name: String, bounds: List[JRefType], compiler: JCompiler) extends JRefType {
  def matches (v: MetaValue): Boolean = v match {
    case that: JRefType  => bounds.forall(that <:< _)
    case that: JWildcard => bounds.forall(ub => that.upperBound.exists(_ <:< ub))
    case _: PureValue => false
  }

  def isAssignableTo (that: JType): Boolean = isSubtypeOf(that)

  def isSubtypeOf (that: JType): Boolean = bounds.exists(_.isSubtypeOf(that)) | compiler.typeLoader.objectType.contains(that)

  def methods = boundHead.map(_.methods).getOrElse(Map.empty)
  def fields = boundHead.map(_.fields).getOrElse(Map.empty)

  lazy val boundHead = bounds.headOption.orElse(compiler.typeLoader.objectType)
}
