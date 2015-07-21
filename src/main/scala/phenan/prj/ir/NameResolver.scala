package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._
import phenan.prj.exception._
import phenan.util.TryUtil._

import scala.util._

import scalaz.Memo._

trait NameResolver {

  def environment: Map[String, MetaArgument]

  def resolve (name: String): Try[JClass]
  def priority (name: String): Option[JPriority]
  def root: RootResolver

  def isTypeVariable (name: String): Boolean
  def isMetaVariable (name: String): Boolean

  def typeVariable (name: String): Option[JTypeVariable] = environment.get(name).collect { case v: JTypeVariable => v }
  def metaVariable (name: String): Option[MetaVariableRef] = environment.get(name).collect { case v: MetaVariableRef => v }

  def withMetaParameter (param: FormalMetaParameter): NameResolver = new NameResolver_MetaParameter (param, this)
  def withInnerClasses (inners: List[IRModule]): NameResolver = new NameResolver_InnerClasses (inners, this)
  def withPriorities (priorities: Set[JPriority]): NameResolver = new NameResolver_Priorities (priorities, this)

  def resolve (name: List[String]): Try[JClass] =
    resolve(name.head).flatMap(root.findInnerClass(_, name.tail)).orElse(root.findClass(name))

  def priority (name: QualifiedName): Option[JPriority] = {
    if (name.names.size > 1) resolve(name.names.init) match {
      case Success(clazz) => clazz.declaredPriorities.find(_.name == name.names.last)
      case Failure(e) => root.compiler.state.errorAndReturn("invalid priority " + name, e, None)
    }
    else name.names.headOption.flatMap(priority)
  }

  def constraint (names: List[QualifiedName]): List[JPriority] = resolveConstraint (names, Nil)

  import scalaz.Scalaz._

  def metaParameter (mp: MetaParameter): Try[FormalMetaParameter] = mp match {
    case TypeParameter(name, bounds)       => bounds.traverse(typeSignature).map(FormalMetaParameter(name, JTypeSignature.typeTypeSig, None, _))
    case MetaValueParameter(name, mt, pri) => typeSignature(mt).map(FormalMetaParameter(name, _, pri.flatMap(priority), Nil))
  }

  def typeSignature (tn: TypeName): Try[JTypeSignature] = nonArrayTypeSignature(tn.name, tn.args).map(JTypeSignature.arraySig(_, tn.dim))

  def classTypeSignature (tn: TypeName): Try[JClassTypeSignature] = {
    if (tn.dim > 0) Failure(InvalidTypeException("expected class type, but found array type : " + tn.name.names.mkString(".") + "[]".multiply(tn.dim)))
    else classTypeSignature(tn.name, tn.args)
  }

  def classTypeSignature (name: String): Try[JClassTypeSignature] = resolve(name).map(clazz => SimpleClassTypeSignature(clazz.internalName, Nil))

  def classTypeSignature (name: QualifiedName): Try[JClassTypeSignature] = classTypeSignature(name.names)

  def classTypeSignature (names: List[String]): Try[JClassTypeSignature] = resolve(names).map(clazz => SimpleClassTypeSignature(clazz.internalName, Nil))

  def classTypeSignature (name: QualifiedName, args: List[TypeArgument]): Try[JClassTypeSignature] = for {
    clazz <- resolve(name.names)
    as    <- args.traverse(typeArgument)
  } yield SimpleClassTypeSignature(clazz.internalName, as)

  def parameterSignature (param: FormalParameter, initializer: Option[String]): Try[JParameterSignature] = {
    parameterSignature(Nil, param.parameterType, param.priority, param.varArgs, param.dim, initializer)
  }

  private def parameterSignature (contexts: List[TypeName], parameterType: ParameterType, pri: Option[QualifiedName], varArgs: Boolean, dim: Int, initializer: Option[String]): Try[JParameterSignature] = parameterType match {
    case ContextualType(c, p) => parameterSignature(contexts :+ c, p, pri, varArgs, dim, initializer)
    case tn: TypeName => for {
      cs  <- contexts.traverse(typeSignature)
      sig <- typeSignature(tn).map(JTypeSignature.arraySig(_, dim))
    } yield JParameterSignature(cs, sig, pri.flatMap(priority), varArgs, initializer)
  }

  private def nonArrayTypeSignature (name: QualifiedName, args: List[TypeArgument]): Try[JTypeSignature] = {
    if (args.nonEmpty) classTypeSignature(name, args)
    else if (name.names.size > 1) classTypeSignature(name)
    else {
      val simpleName = name.names.head
      if (isTypeVariable(simpleName)) Success(JTypeVariableSignature(simpleName))
      else primitiveType(simpleName).orElse(classTypeSignature(simpleName))
    }
  }

  private def primitiveType (name: String): Try[JPrimitiveTypeSignature] = name match {
    case "byte"    => Success(ByteTypeSignature)
    case "char"    => Success(CharTypeSignature)
    case "double"  => Success(DoubleTypeSignature)
    case "float"   => Success(FloatTypeSignature)
    case "int"     => Success(IntTypeSignature)
    case "long"    => Success(LongTypeSignature)
    case "short"   => Success(ShortTypeSignature)
    case "boolean" => Success(BoolTypeSignature)
    case "void"    => Success(VoidTypeSignature)
    case _ => Failure(InvalidTypeException("type " + name + " is not a primitive type"))
  }

  private def typeArgument (arg: TypeArgument): Try[JTypeArgument] = arg match {
    case TypeName(QualifiedName(List(name)), Nil, 0) if isMetaVariable(name) => Success(MetaVariableSignature(name))
    case tn: TypeName                  => typeSignature(tn)
    case UpperBoundWildcardType(bound) => typeSignature(bound).map(ub => WildcardArgument(Some(ub), None))
    case LowerBoundWildcardType(bound) => typeSignature(bound).map(lb => WildcardArgument(None, Some(lb)))
    case UnboundWildcardType           => Success(WildcardArgument(None, None))
  }

  private def resolveConstraint (c: List[QualifiedName], result: List[JPriority]): List[JPriority] = c match {
    case p :: rest => priority(p) match {
      case Some(r) => resolveConstraint(rest, r :: result)
      case None    =>
        root.compiler.state.error("invalid priority name : " + p)
        resolveConstraint(rest, result)
    }
    case Nil => result.reverse
  }
}

object NameResolver {
  def root (compiler: JCompiler) = new RootResolver(compiler)
}

class RootResolver private[ir] (val compiler: JCompiler) extends NameResolver {
  def file (file: IRFile): NameResolver = NameResolverInFile(file, this)

  def environment: Map[String, MetaArgument] = Map.empty

  def isTypeVariable (name: String) = false

  def isMetaVariable (name: String) = false

  def resolve (name: String): Try[JClass] = abbreviated(name).orElse(findClass(name))

  def priority (name: String): Option[JPriority] = None

  def findClass (name: List[String]): Try[JClass] = {
    if (name.size > 1) findClass(List(name.head), name.tail.head, name.tail.tail)
    else resolve(name.head)
  }

  def isKnownPackage (names: List[String]) = knownPackages.contains(names)

  def findClass (packageName: List[String], name: String, rest: List[String]): Try[JClass] = {
    val names = packageName :+ name
    if (isKnownPackage(names)) findClass(names, rest.head, rest.tail)
    else findClass(names.mkString("/")) match {
      case Success(clazz) =>
        addKnownPackages(packageName)
        findInnerClass(clazz, rest)
      case fail => rest match {
        case head :: tail => findClass(names, head, tail)
        case Nil          => fail
      }
    }
  }

  def addKnownPackages (packageName: List[String]): Unit = {
    if (! isKnownPackage(packageName)) {
      knownPackages += packageName
      if (packageName.size > 1) addKnownPackages(packageName.init)
    }
  }

  def findInnerClass (clazz: JClass, name: List[String]): Try[JClass] = name match {
    case head :: tail => findInnerClass(clazz, head) match {
      case Success(inner) => findInnerClass(inner, tail)
      case fail => fail
    }
    case Nil => Success(clazz)
  }

  def findInnerClass (clazz: JClass, name: String): Try[JClass] = compiler.classLoader.loadInnerClass(clazz, name)
  def findClass (name: String): Try[JClass] = compiler.classLoader.loadClass(name)

  def root = this

  private val abbreviated: String => Try[JClass] = mutableHashMapMemo { name =>
    findClass("java/lang/" + name).orElse(findClass("proteaj/lang/" + name))
  }

  private val knownPackages: scala.collection.mutable.Set[List[String]] = scala.collection.mutable.Set (
    List("java"), List("java", "lang"), List("java", "util"), List("java", "io"),
    List("proteaj"), List("proteaj", "lang")
  )
}

case class NameResolverInFile (file: IRFile, root: RootResolver) extends NameResolver {
  def environment: Map[String, MetaArgument] = Map.empty

  def isTypeVariable (name: String) = false

  def isMetaVariable(name: String) = false

  def resolve (name: String): Try[JClass] = abbreviated(name).orElse(root.resolve(name))

  def priority (name: String): Option[JPriority] = root.priority(name)

  private lazy val packageName = file.packageName.map(_.names)

  private lazy val importedClasses: Map[String, List[String]] = file.importedClassNames.map { name => name.names.last -> name.names }.toMap

  private lazy val importedPackages: List[List[String]] = file.importedPackageNames.map(_.names)

  private val abbreviated: String => Try[JClass] = mutableHashMapMemo { name =>
    if (importedClasses.contains(name)) root.findClass(importedClasses(name))
    else packageName match {
      case Some(pack) => root.findClass(pack, name, Nil).orElse(searchImportedPackages(name, importedPackages))
      case None       => searchImportedPackages(name, importedPackages)
    }
  }

  private def searchImportedPackages (name: String, imported: List[List[String]]): Try[JClass] = imported match {
    case pack :: rest => root.findClass(pack, name, Nil) match {
      case s: Success[_] => s
      case _: Failure[_] => searchImportedPackages(name, rest)
    }
    case Nil => Failure(ClassFileNotFoundException("class name " + name + " cannot be resolved"))
  }
}

class NameResolver_MetaParameter (metaParameter: FormalMetaParameter, parent: NameResolver) extends NameResolver {
  lazy val environment: Map[String, MetaArgument] = parent.environment + (metaParameter.name -> metaValue)

  def isMetaVariable (name: String): Boolean = (metaParameter.name == name && metaParameter.metaType != JTypeSignature.typeTypeSig) || parent.isMetaVariable(name)

  def isTypeVariable (name: String): Boolean = (metaParameter.name == name && metaParameter.metaType == JTypeSignature.typeTypeSig) || parent.isTypeVariable(name)

  def resolve (name: String): Try[JClass] = parent.resolve(name)

  def priority (name: String): Option[JPriority] = parent.priority(name)

  def root: RootResolver = parent.root

  import scalaz.Scalaz._
  private def metaValue: MetaArgument = if (metaParameter.metaType == JTypeSignature.typeTypeSig) {
    compiler.state.someOrError(metaParameter.bounds.traverse(compiler.typeLoader.fromTypeSignature_RefType(_, parent.environment)).map(JTypeVariable(metaParameter.name, _, compiler)),
      "invalid type bounds : " + metaParameter.bounds, JTypeVariable(metaParameter.name, Nil, compiler))
  } else {
    compiler.state.someOrError(compiler.typeLoader.fromTypeSignature(metaParameter.metaType, parent.environment).map(MetaVariableRef(metaParameter.name, _)),
      "invalid meta type : " + metaParameter.metaType, JTypeVariable(metaParameter.name, Nil, compiler))
  }

  private def compiler = root.compiler
}

class NameResolver_InnerClasses (inners: List[IRModule], parent: NameResolver) extends NameResolver {
  def environment: Map[String, MetaArgument] = parent.environment

  def isMetaVariable (name: String): Boolean = parent.isMetaVariable(name)
  def isTypeVariable (name: String): Boolean = parent.isTypeVariable(name)

  def resolve (name: String): Try[JClass] = abbreviated(name).map(Success(_)).getOrElse(parent.resolve(name))

  def priority (name: String): Option[JPriority] = parent.priority(name)

  def root: RootResolver = parent.root

  private val abbreviated: String => Option[JClass] = mutableHashMapMemo { name => inners.find(_.simpleName == name) }
}

class NameResolver_Priorities (priorities: Set[JPriority], parent: NameResolver) extends NameResolver {
  def environment: Map[String, MetaArgument] = parent.environment

  def isMetaVariable (name: String): Boolean = parent.isMetaVariable(name)
  def isTypeVariable (name: String): Boolean = parent.isTypeVariable(name)

  def resolve (name: String): Try[JClass] = parent.resolve(name)

  def priority (name: String): Option[JPriority] = abbreviated(name).orElse(parent.priority(name))

  def root: RootResolver = parent.root

  private val abbreviated: String => Option[JPriority] = mutableHashMapMemo { name => priorities.find(_.name == name) }
}
