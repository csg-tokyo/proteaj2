package phenan.prj

import phenan.prj.ir.IRExpressions

trait JModules {
  this: JTypeLoader with IRExpressions with Syntax with JMembers with JErasedTypes =>

  sealed trait MetaArgument {
    def name: String

    def matches(v: MetaArgument): Boolean
  }

  type MetaArgs = Map[String, MetaArgument]

  sealed trait MetaValue extends MetaArgument {
    def valueType: JType
  }

  case class MetaVariableRef(name: String, valueType: JType) extends MetaValue {
    def matches(v: MetaArgument): Boolean = this == v
  }

  case class ConcreteMetaValue(ast: IRExpression, valueType: JType) extends MetaValue {
    def name: String = ast.toString

    override def matches(v: MetaArgument): Boolean = this == v
  }

  case class JUnboundMetaVariable(valueType: JType) extends MetaValue {
    def name: String = "?:" + valueType.name

    def matches(v: MetaArgument): Boolean = v match {
      case m: MetaValue => m.valueType <:< valueType
      case _: JWildcard | _: JRefType => false
    }
  }

  case class JWildcard(upperBound: Option[JRefType], lowerBound: Option[JRefType]) extends MetaArgument {
    def name: String = upperBound.map(ub => "? extends " + ub.name).orElse(lowerBound.map(lb => "? super " + lb.name)).getOrElse("?")

    def matches(that: MetaArgument): Boolean = that match {
      case that: JRefType => upperBound.forall(that <:< _) && lowerBound.forall(_ <:< that)
      case that: JWildcard => upperBound.forall(ub => that.upperBound.exists(_ <:< ub)) && lowerBound.forall(lb => that.lowerBound.exists(lb <:< _))
      case _: MetaValue => false
    }
  }

  case class JGenericType(signature: JTypeSignature, env: MetaArgs) {
    def bind(args: MetaArgs): Option[JType] = {
      fromTypeSignature(signature, env ++ args)
    }

    def unbound(args: MetaArgs): Set[String] = unbound(signature, args, Set.empty[String])

    override def toString: String = signature.toString + env.map { case (s, m) => s + " = " + m.name }.mkString("<", ",", ">")

    private def unbound(sig: JTypeSignature, args: MetaArgs, result: Set[String]): Set[String] = sig match {
      case JTypeVariableSignature(name) if args.contains(name) || env.contains(name) => result
      case JTypeVariableSignature(name) => result + name
      case SimpleClassTypeSignature(_, as) => as.foldLeft(result) { (r, a) => unbound(a, args, r) }
      case MemberClassTypeSignature(outer, _, as) => as.foldLeft(unbound(outer, args, result)) { (r, a) => unbound(a, args, r) }
      case JArrayTypeSignature(component) => unbound(component, args, result)
      case JCapturedWildcardSignature(ub, lb) => ub.map(unbound(_, args, result)).orElse(lb.map(unbound(_, args, result))).getOrElse(result)
      case _: JPrimitiveTypeSignature => result
    }

    private def unbound(sig: JTypeArgument, args: MetaArgs, result: Set[String]): Set[String] = sig match {
      case MetaVariableSignature(name) if args.contains(name) || env.contains(name) => result
      case MetaVariableSignature(name) => result + name
      case sig: JTypeSignature => unbound(sig, args, result)
      case WildcardArgument(ub, lb) => ub.map(unbound(_, args, result)).orElse(lb.map(unbound(_, args, result))).getOrElse(result)
    }
  }

  sealed trait JModule {
    protected def sortMethods(methods: List[JMethod]): List[JMethod] = methods.sortWith { (m1, m2) =>
      m1.erasedParameterTypes.size > m2.erasedParameterTypes.size ||
        (m1.erasedParameterTypes.size == m2.erasedParameterTypes.size && compareMethodParams(m1.erasedParameterTypes.zip(m2.erasedParameterTypes)))
    }

    private def compareMethodParams(params: List[(JErasedType, JErasedType)]): Boolean = params match {
      case (p1, p2) :: rest => p1.isSubclassOf(p2) || compareMethodParams(rest)
      case Nil => false
    }
  }

  case class JClassModule(clazz: JClass) extends JModule {
    def findField(name: String, from: JClass): Option[JField] = {
      if (clazz == from) privateFields.get(name).orElse(fields.get(name))
      else if (clazz.packageInternalName == from.packageInternalName) fields.get(name)
      else fields.get(name).filter(_.isPublic)
    }

    def findMethod(name: String, from: JClass): List[JMethod] = sortMethods {
      if (clazz == from) privateMethods.getOrElse(name, Nil) ++ methods.getOrElse(name, Nil)
      else if (clazz.packageInternalName == from.packageInternalName) methods.getOrElse(name, Nil)
      else methods.getOrElse(name, Nil).filter(_.isPublic)
    }

    def findPublicMethod (name: String): List[JMethod] = sortMethods {
      methods.getOrElse(name, Nil).filter(_.isPublic)
    }

    lazy val declaredFields: List[JField] = clazz.fields.filter(_.isStatic).flatMap { fieldDef =>
      fromTypeSignature(fieldDef.signature, Map.empty).map(fieldType => JField(fieldDef, fieldType, this))
    }

    lazy val fields: Map[String, JField] = declaredFields.filterNot(_.isPrivate).map(f => f.name -> f).toMap
    lazy val privateFields: Map[String, JField] = declaredFields.filter(_.isPrivate).map(f => f.name -> f).toMap

    lazy val declaredMethods: List[JMethod] = clazz.methods.filter(_.isStaticMethod).map { methodDef =>
      new JMethod(methodDef, Map.empty, this)
    }

    lazy val methods: Map[String, List[JMethod]] = declaredMethods.filterNot(_.isPrivate).groupBy(_.name)
    lazy val privateMethods: Map[String, List[JMethod]] = declaredMethods.filter(_.isPrivate).groupBy(_.name)

    def priorities: Set[JPriority] = clazz.priorities

    def constraints: List[List[JPriority]] = clazz.priorityConstraints

    lazy val withDSLs: List[JClassModule] = clazz.withDSLs.map(_.classModule)

    lazy val expressionOperators: List[(JExpressionSyntax, JMethod)] = collectExpressionOperators(declaredMethods.filterNot(_.isPrivate), Nil)
    lazy val literalOperators: List[(JLiteralSyntax, JMethod)] = collectLiteralOperators(declaredMethods.filterNot(_.isPrivate), Nil)

    private def collectExpressionOperators(ms: List[JMethod], es: List[(JExpressionSyntax, JMethod)]): List[(JExpressionSyntax, JMethod)] = ms match {
      case m :: rest => m.syntax match {
        case Some(s: JExpressionSyntax) => collectExpressionOperators(rest, (s -> m) :: es)
        case _ => collectExpressionOperators(rest, es)
      }
      case Nil => es
    }

    private def collectLiteralOperators(ms: List[JMethod], es: List[(JLiteralSyntax, JMethod)]): List[(JLiteralSyntax, JMethod)] = ms match {
      case m :: rest => m.syntax match {
        case Some(s: JLiteralSyntax) => collectLiteralOperators(rest, (s -> m) :: es)
        case _ => collectLiteralOperators(rest, es)
      }
      case Nil => es
    }
  }

  sealed trait JType extends JModule {
    def name: String

    def array: JArrayType = arrayTypeOf(this)

    def array(dim: Int): JType = {
      if (dim > 0) array.array(dim - 1)
      else this
    }

    def isSubtypeOf(that: JType): Boolean

    def <:<(t: JType): Boolean = this.isSubtypeOf(t)

    def >:>(t: JType): Boolean = t.isSubtypeOf(this)

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField]

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod]
  }

  sealed trait JRefType extends JType with MetaArgument

  case class JObjectType(erase: JClass, env: MetaArgs) extends JRefType {
    def name: String = {
      if (env.isEmpty) erase.name
      else erase.name + env.map(kv => kv._1 + "=" + kv._2.name).mkString("<", ",", ">")
    }

    override def toString: String = name

    def superType: Option[JObjectType] = fromClassTypeSignature(erase.signature.superClass, env)

    def interfaceTypes: List[JObjectType] = erase.signature.interfaces.flatMap(fromClassTypeSignature(_, env))

    lazy val superTypes: List[JObjectType] = superType match {
      case Some(sup) if sup != this => sup :: interfaceTypes
      case _ => interfaceTypes
    }

    lazy val constructors: List[JConstructor] = {
      val cs = erase.methods.filter(_.isConstructor).map { constructorDef => new JConstructor(constructorDef, env, this) }
      if (cs.isEmpty && erase.isClass) List(createDefaultConstructor)
      else cs
    }

    def createDefaultConstructor = new JConstructor(new JMethodDef {
      def syntax: Option[JSyntaxDef] = None

      def declaringClass: JClass = erase

      def name: String = CommonNames.constructorName

      def signature: JMethodSignature = JMethodSignature(Nil, Nil, VoidTypeSignature, Nil, Nil, Nil, Nil, Nil)

      def mod: JModifier = JModifier(JModifier.accPublic)
    }, env, this)

    lazy val declaredFields: List[JField] = erase.fields.filterNot(_.isStatic).flatMap { fieldDef =>
      fromTypeSignature(fieldDef.signature, env).map(fieldType => JField(fieldDef, fieldType, this))
    }

    lazy val declaredMethods: List[JMethod] = {
      erase.methods.filter(_.isInstanceMethod).map { methodDef => new JMethod(methodDef, env, this) }
    }

    lazy val privateFields: Map[String, JField] = declaredFields.filter(_.isPrivate).map(f => f.name -> f).toMap
    lazy val privateMethods: Map[String, List[JMethod]] = declaredMethods.filter(_.isPrivate).groupBy(_.name)

    lazy val fields: Map[String, JField] = nonPrivateFieldList.map(f => f.name -> f).toMap
    lazy val methods: Map[String, List[JMethod]] = nonPrivateMethodList.groupBy(_.name).mapValues(filterOutOverriddenMethod)

    def findConstructor(from: JClass): List[JConstructor] = {
      if (erase == from) constructors
      else if (erase.packageInternalName == from.packageInternalName) constructors.filter(!_.isPrivate)
      else constructors.filter(_.isPublic)
    }

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = {
      if (erase == from) privateFields.get(name).orElse(fields.get(name))
      else if (erase.packageInternalName == from.packageInternalName) fields.get(name)
      else if (from.isSubclassOf(erase) && receiverIsThis) fields.get(name).filter(f => f.isProtected || f.isPublic)
      else fields.get(name).filter(_.isPublic)
    }

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod] = sortMethods {
      if (erase == from) privateMethods.getOrElse(name, Nil) ++ methods.getOrElse(name, Nil)
      else if (erase.packageInternalName == from.packageInternalName) methods.getOrElse(name, Nil)
      else if (from.isSubclassOf(erase) && receiverIsThis) methods.getOrElse(name, Nil).filter(m => m.isProtected || m.isPublic)
      else methods.getOrElse(name, Nil).filter(_.isPublic)
    }

    def isSubtypeOf(that: JType): Boolean = that match {
      case _ if this == that => true
      case that: JObjectType => isSubtypeOf(that)
      case that: JCapturedWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
      case that: JUnboundTypeVariable =>
        if (that.bounds.isEmpty) isSubtypeOf(objectType)
        else that.bounds.exists(b => isSubtypeOf(b))
      case _: JPrimitiveType | _: JArrayType | _: JTypeVariable => false
    }

    def isSubtypeOf(that: JObjectType): Boolean = {
      (this.erase == that.erase && matchTypeArgs(that.env)) ||
        superTypes.exists { t => this != t && t.isSubtypeOf(that) }
    }

    def matches(that: MetaArgument): Boolean = this == that

    lazy val expressionOperators: List[(JExpressionSyntax, JMethod)] = collectExpressionOperators(nonPrivateMethodList, Nil)
    lazy val literalOperators: List[(JLiteralSyntax, JMethod)] = collectLiteralOperators(nonPrivateMethodList, Nil)

    private def collectExpressionOperators(ms: List[JMethod], es: List[(JExpressionSyntax, JMethod)]): List[(JExpressionSyntax, JMethod)] = ms match {
      case m :: rest => m.syntax match {
        case Some(s: JExpressionSyntax) => collectExpressionOperators(rest, (s -> m) :: es)
        case _ => collectExpressionOperators(rest, es)
      }
      case Nil => es
    }

    private def collectLiteralOperators(ms: List[JMethod], es: List[(JLiteralSyntax, JMethod)]): List[(JLiteralSyntax, JMethod)] = ms match {
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

    private def filterOutOverriddenMethod(list: List[JMethod]): List[JMethod] = {
      list.foldRight[List[JMethod]](Nil) { (m, ms) =>
        if (ms.exists(_.overrides(m))) ms
        else m :: ms
      }
    }

    private def matchTypeArgs(args: MetaArgs): Boolean = env.forall { case (key, value) =>
      args.get(key).exists { arg => arg.matches(value) }
    }
  }

  case class JPrimitiveType(clazz: JPrimitiveClass) extends JType {
    def name: String = clazz.name

    def methods: Map[String, List[JMethod]] = Map.empty

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = None

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod] = Nil

    def isSubtypeOf(that: JType): Boolean = this == that
  }

  case class JArrayType(componentType: JType) extends JRefType {
    def name: String = componentType.name + "[]"

    def superTypes: List[JObjectType] = superTypesOfArray

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = None

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod] = Nil

    def isSubtypeOf(that: JType): Boolean = that match {
      case _ if this == that => true
      case that: JArrayType => componentType.isSubtypeOf(that.componentType)
      case that: JObjectType => superTypes.exists(_.isSubtypeOf(that))
      case that: JCapturedWildcardType => that.lowerBound.exists(lb => isSubtypeOf(lb))
      case _: JPrimitiveType | _: JUnboundTypeVariable => false
    }

    def matches(that: MetaArgument): Boolean = this == that
  }

  case class JTypeVariable(name: String, bounds: List[JRefType]) extends JRefType {
    def isSubtypeOf(that: JType): Boolean = this == that || bounds.exists(_.isSubtypeOf(that)) || objectType == that

    override def matches(v: MetaArgument): Boolean = this == v

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = findField_helper(bounds, name, from, receiverIsThis)

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod] = bounds.foldRight(List.empty[JMethod]) { (bound, methods) =>
      bound.findMethod(name, from, receiverIsThis) ++ methods
    }

    private def findField_helper(bounds: List[JRefType], name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = bounds match {
      case bound :: rest => bound.findField(name, from, receiverIsThis) match {
        case None => findField_helper(rest, name, from, receiverIsThis)
        case r => r
      }
      case Nil => None
    }
  }

  case class JCapturedWildcardType private(upperBound: JRefType, lowerBound: Option[JRefType], id: Int) extends JRefType {
    override def name: String = "capture#" + id

    override def isSubtypeOf(that: JType): Boolean = upperBound.isSubtypeOf(that)

    override def matches(v: MetaArgument): Boolean = this == v

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = upperBound.findField(name, from, receiverIsThis)

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod] = upperBound.findMethod(name, from, receiverIsThis)
  }

  object JCapturedWildcardType {
    def apply(upperBound: JRefType, lowerBound: Option[JRefType]): JCapturedWildcardType = {
      val cap = JCapturedWildcardType(upperBound, lowerBound, id)
      id += 1
      cap
    }

    private var id = 0
  }

  case class JUnboundTypeVariable(name: String, bounds: List[JRefType]) extends JRefType {
    def matches(v: MetaArgument): Boolean = v match {
      case that: JRefType => bounds.forall(that <:< _)
      case that: JWildcard => bounds.forall(ub => that.upperBound.exists(_ <:< ub))
      case _: MetaValue => false
    }

    def isSubtypeOf(that: JType): Boolean = bounds.exists(_.isSubtypeOf(that)) || objectType == that

    def findField(name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = findField_helper(bounds, name, from, receiverIsThis)

    def findMethod(name: String, from: JClass, receiverIsThis: Boolean): List[JMethod] = bounds.foldRight(List.empty[JMethod]) { (bound, methods) =>
      bound.findMethod(name, from, receiverIsThis) ++ methods
    }

    private def findField_helper(bounds: List[JRefType], name: String, from: JClass, receiverIsThis: Boolean): Option[JField] = bounds match {
      case bound :: rest => bound.findField(name, from, receiverIsThis) match {
        case None => findField_helper(rest, name, from, receiverIsThis)
        case r => r
      }
      case Nil => None
    }

    lazy val boundHead: JRefType = bounds.headOption.getOrElse(objectType)
  }

}