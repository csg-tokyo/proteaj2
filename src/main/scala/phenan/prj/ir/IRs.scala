package phenan.prj.ir

import phenan.prj._
import phenan.prj.JCompiler
import phenan.prj.declaration._
import phenan.util._

import JModifier._

case class IRFile (ast: CompilationUnit, root: RootResolver) {
  lazy val modules: List[IRClass] = collectModules(ast.modules.map(IRClass(_, this)), Nil)

  lazy val internalName = ast.header.pack.map(_.name.names.mkString("/"))

  lazy val resolver = root.file(this)

  private def collectModules (modules: List[IRClass], result: List[IRClass]): List[IRClass] = modules match {
    case m :: ms => collectModules(ms ++ m.inners, result :+ m)
    case Nil     => result
  }

  def compiler = root.compiler
  def state = compiler.state
}

trait IRClass extends JClass {
  def file: IRFile

  def outer: Option[IRClass]
  def inners: List[IRClass]

  def simpleName: String
  def superTypeSignature: JClassTypeSignature
  def interfaceSignatures: List[JClassTypeSignature]

  private[ir] def implicitClassModifier: Int
  private[ir] def implicitMethodModifier: Int
  private[ir] def implicitFieldModifier: Int

  private[ir] def modifiersAST: List[Modifier]
  private[ir] def metaParametersAST: List[MetaParameter]

  lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitClassModifier

  lazy val annotations = modifiersAST.collect { case ann: Annotation => ann }

  lazy val name = internalName.replace('/', '.').replace('$', '.')

  lazy val outerClass: Option[String] = outer.map(_.internalName)
  
  lazy val internalName = outerClass match {
    case Some(outer) => outer + '$' + simpleName
    case None        => file.internalName.map(_ + '/').getOrElse("") + simpleName
  }

  lazy val innerClasses = inners.map(m => m.simpleName -> m.internalName).toMap

  lazy val signature: JClassSignature = JClassSignature(formalMetaParameters, superTypeSignature, interfaceSignatures)

  import scalaz.Scalaz._

  lazy val formalMetaParameters = state.successOrError(metaParametersAST.traverse(resolver.metaParameter), "invalid meta parameters of class " + name, Nil)

  lazy val resolver = file.resolver.inClass(this)
  
  def compiler: JCompiler = file.compiler
}

object IRClass {
  def apply (module: ModuleDeclaration, file: IRFile): IRClass = apply(module, None, file)
  def apply (module: ModuleDeclaration, outer: Option[IRClass], file: IRFile): IRClass = module match {
    case c: ClassDeclaration => IRClassDef(c, outer, file)
    case i: InterfaceDeclaration => IRInterfaceDef(i, outer, file)
    case _ => ???
  }
}

trait IRClassLike extends IRClass {
  private[ir] def defaultSuperTypeSignature: JClassTypeSignature

  private[ir] def membersAST: List[ClassMember]
  private[ir] def superTypeAST: Option[TypeName]
  private[ir] def interfacesAST: List[TypeName]

  lazy val inners: List[IRClass] = membersAST.collect {
    case m: ModuleDeclaration => IRClass(m, Some(this), file)
  }

  lazy val fields: List[JFieldDef] = membersAST.collect {
    case FieldDeclaration(mods, ft, ds) =>
      val mod = IRModifiers.mod(mods)
      val fieldType = state.successOrError(resolver.typeSignature(ft), "invalid type of field " + ds.head.name, JTypeSignature.objectTypeSig)
      ds.map(IRFieldDef(mod, fieldType, _, this))
  }.flatten

  lazy val methods: List[IRMethod] = collectMethods(declaredMethods ++ declaredConstructors ++ instanceInitializer ++ staticInitializer)

  lazy val declaredMethods = membersAST.collect { case m: MethodDeclaration => IRMethodDef(m, this) }

  lazy val declaredConstructors = membersAST.collect { case c: ConstructorDeclaration => IRConstructorDef(c, this) }

  lazy val instanceInitializer = membersAST.collect { case i: InstanceInitializer => IRInstanceInitializer(i, this) }

  lazy val staticInitializer = membersAST.collect { case s: StaticInitializer => IRStaticInitializer(s, this) }

  import scalaz.Scalaz._

  lazy val superTypeSignature = superTypeAST.map { s =>
    state.successOrError(resolver.classTypeSignature(s), "invalid super type of class " + name + " : " + s, defaultSuperTypeSignature)
  }.getOrElse(defaultSuperTypeSignature)

  lazy val interfaceSignatures = state.successOrError(interfacesAST.traverse(resolver.classTypeSignature), "invalid interface types of class " + name, Nil)

  def dslInfo = None

  private def collectMethods (methods: List[IRMethod]): List[IRMethod] = methods ++ methods.flatMap(_.paramInitializers)
}

case class IRClassDef (ast: ClassDeclaration, outer: Option[IRClass], file: IRFile) extends IRClassLike {
  def simpleName: String = ast.name

  private[ir] def implicitClassModifier = accSuper
  private[ir] def implicitMethodModifier = 0
  private[ir] def implicitFieldModifier = 0
  private[ir] def defaultSuperTypeSignature = JTypeSignature.objectTypeSig

  private[ir] def modifiersAST = ast.modifiers
  private[ir] def metaParametersAST = ast.metaParameters
  private[ir] def superTypeAST = ast.superClass
  private[ir] def interfacesAST = ast.interfaces
  private[ir] def membersAST = ast.members
}

case class IRInterfaceDef (ast: InterfaceDeclaration, outer: Option[IRClass], file: IRFile) extends IRClassLike {
  def simpleName: String = ast.name

  private[ir] def implicitClassModifier = accAbstract | accInterface
  private[ir] def implicitMethodModifier: Int = accPublic | accAbstract
  private[ir] def implicitFieldModifier: Int = accPublic | accStatic | accFinal
  private[ir] def defaultSuperTypeSignature: JClassTypeSignature = JTypeSignature.objectTypeSig

  private[ir] def modifiersAST = ast.modifiers
  private[ir] def metaParametersAST: List[MetaParameter] = ast.metaParameters
  private[ir] def superTypeAST: Option[TypeName] = None
  private[ir] def interfacesAST: List[TypeName] = ast.superInterfaces
  private[ir] def membersAST: List[ClassMember] = ast.members
}

case class IRFieldDef (mod: JModifier, fieldType: JTypeSignature, ast: VariableDeclarator, declaringClass: IRClass) extends JFieldDef {
  def name: String = ast.name
  def signature: JTypeSignature = JTypeSignature.arraySig(fieldType, ast.dim)
}

trait IRMethod extends JMethodDef {
  def declaringClass: IRClass
  def returnTypeSignature: JTypeSignature

  private[ir] def metaParametersAST: List[MetaParameter]
  private[ir] def formalParametersAST: List[FormalParameter]
  private[ir] def clausesAST: List[MethodClause]

  lazy val signature: JMethodSignature =
    JMethodSignature(formalMetaParameters, formalParameterSignatures, returnTypeSignature,
      throwsTypeSignatures, activatesTypeSignatures, deactivatesTypeSignatures, requiresTypeSignatures)

  lazy val resolver = declaringClass.resolver.inMethod(this)

  import scalaz.Scalaz._

  lazy val formalMetaParameters = state.successOrError(metaParametersAST.traverse(resolver.metaParameter), "invalid meta parameters of method " + name, Nil)
  lazy val formalParameters = formalParametersAST.map(param => new IRFormalParameter(param, this))
  lazy val formalParameterSignatures = state.successOrError(formalParameters.traverse(_.signature), "invalid parameter types of method " + name, Nil)

  lazy val throwsTypeSignatures = clausesAST.collectFirst { case ThrowsClause(es) =>
    state.successOrError(es.traverse(resolver.typeSignature), "invalid throws clause of method " + name, Nil)
  }.getOrElse(Nil)

  lazy val activatesTypeSignatures = clausesAST.collectFirst { case ActivatesClause(cs) =>
    state.successOrError(cs.traverse(resolver.typeSignature), "invalid activates clause of method " + name, Nil)
  }.getOrElse(Nil)

  lazy val deactivatesTypeSignatures = clausesAST.collectFirst { case DeactivatesClause(cs) =>
    state.successOrError(cs.traverse(resolver.typeSignature), "invalid deactivates clause of method " + name, Nil)
  }.getOrElse(Nil)

  lazy val requiresTypeSignatures = clausesAST.collectFirst { case RequiresClause(cs) =>
    state.successOrError(cs.traverse(resolver.typeSignature), "invalid requires clause of method " + name, Nil)
  }.getOrElse(Nil)

  private[ir] def paramInitializers: List[IRParameterInitializer] = formalParameters.flatMap(_.initializerMethod)
}

class IRFormalParameter (ast: FormalParameter, method: IRMethod) {
  private lazy val initializer = ast.initializer.map(snippet => (method.name + "$init$" + method.state.uniqueId, snippet))
  lazy val signature = method.resolver.parameterSignature(ast, initializer.map(_._1))
  lazy val initializerMethod = for {
    returnType <- signature.map(_.actualTypeSignature).toOption
    (name, snippet) <- initializer
  } yield IRParameterInitializer(method, returnType, name, snippet)
}

case class IRMethodDef (ast: MethodDeclaration, declaringClass: IRClass) extends IRMethod {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers) | declaringClass.implicitMethodModifier

  def name: String = ast.name

  override def syntax: Option[JOperatorSyntaxDef] = None

  lazy val returnTypeSignature = state.successOrError(resolver.typeSignature(ast.returnType), "invalid return type of method " + name, VoidTypeSignature)

  private[ir] def metaParametersAST: List[MetaParameter] = ast.metaParameters
  private[ir] def formalParametersAST: List[FormalParameter] = ast.formalParameters
  private[ir] def clausesAST: List[MethodClause] = ast.clauses
}

case class IRConstructorDef (ast: ConstructorDeclaration, declaringClass: IRClass) extends IRMethod {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers)

  def name = CommonNames.constructorName

  override def returnTypeSignature: JTypeSignature = VoidTypeSignature

  override def syntax: Option[JOperatorSyntaxDef] = None

  override private[ir] def metaParametersAST: List[MetaParameter] = ast.metaParameters
  override private[ir] def formalParametersAST: List[FormalParameter] = ast.formalParameters
  override private[ir] def clausesAST: List[MethodClause] = ast.clauses
}

trait IRInitializerMethod extends IRMethod {
  private[ir] def metaParametersAST: List[MetaParameter] = Nil
  private[ir] def formalParametersAST: List[FormalParameter] = Nil
  private[ir] def clausesAST: List[MethodClause] = Nil

  override def syntax: Option[JOperatorSyntaxDef] = None
}

case class IRInstanceInitializer (ast: InstanceInitializer, declaringClass: IRClass) extends IRInitializerMethod {
  def mod: JModifier = JModifier(0)
  def name: String = ""
  def returnTypeSignature: JTypeSignature = VoidTypeSignature
}

case class IRStaticInitializer (ast: StaticInitializer, declaringClass: IRClass) extends IRInitializerMethod {
  def mod: JModifier = JModifier(accStatic)
  def name: String = CommonNames.classInitializerName
  def returnTypeSignature: JTypeSignature = VoidTypeSignature
}

case class IRParameterInitializer (method: IRMethod, returnTypeSignature: JTypeSignature, name: String, snippet: ExpressionSnippet) extends IRInitializerMethod {
  def mod = JModifier(accPublic | accStatic | accFinal)
  def declaringClass: IRClass = method.declaringClass
}

object IRModifiers {
  def mod (modifiers: List[Modifier]): JModifier = JModifier(modifiers.foldRight(0)((m, flags) => flags | flag(m)))

  private def flag (modifier: Modifier): Int = modifier match {
    case PublicModifier       => accPublic
    case PrivateModifier      => accPrivate
    case ProtectedModifier    => accProtected
    case StaticModifier       => accStatic
    case FinalModifier        => accFinal
    case SynchronizedModifier => accSynchronized
    case VolatileModifier     => accVolatile
    case TransientModifier    => accTransient
    case NativeModifier       => accNative
    case AbstractModifier     => accAbstract
    case StrictFPModifier     => accStrict
    case _ => 0
  }
}

// TODO:
trait IRAnnotations {
  def resolver: NameResolver
  def annotations: List[Annotation]

  def classSig: Option[JClassSignature] = find (JTypeSignature.classSigTypeSig).map { args =>
    val metaParams = args.get("metaParameters")
    val superType = args.get("superType")
    val interfaces = args.get("interfaces")
    ???
  }

  def find (sig: JClassTypeSignature): Option[Map[String, AnnotationElement]] = annotations.collectFirst {
    case MarkerAnnotation(name) if resolver.classTypeSignature(name).toOption.contains(sig) => Map.empty
    case SingleElementAnnotation(name, arg) if resolver.classTypeSignature(name).toOption.contains(sig) => Map("value" -> arg)
    case FullAnnotation(name, args) if resolver.classTypeSignature(name).toOption.contains(sig) => args
  }

  type ElemReader[A] = scalaz.ReaderT[Option, AnnotationElement, A]
  object ElemReader extends scalaz.KleisliInstances with scalaz.KleisliFunctions {
    def apply[A] (f: AnnotationElement => Option[A]): ElemReader[A] = kleisli(f)
  }

}
