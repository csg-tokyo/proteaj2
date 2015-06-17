package phenan.prj.ir

import phenan.prj._
import phenan.prj.JCompiler
import phenan.prj.declaration._

import scala.util._

import JModifier._

case class IRFile (ast: CompilationUnit, root: RootResolver) {
  lazy val modules: List[IRClass] = collectModules(ast.modules.map(IRClass(_, this)), Nil)

  lazy val internalName = ast.header.pack.map(_.name.names.mkString("/"))
  
  private def collectModules (modules: List[IRClass], result: List[IRClass]): List[IRClass] = modules match {
    case m :: ms => collectModules(ms ++ m.inners, result :+ m)
    case Nil     => result
  }

  lazy val resolver = root.file(this)
  def compiler = root.compiler
  def state = compiler.state

  private[ir] def annotationReader = new IRAnnotationReader(this)
}

sealed trait IRMember

trait IRClass extends JClass with IRMember {
  def file: IRFile

  def outer: Option[IRClass]
  def declaredMembers: List[IRMember]

  def simpleName: String
  
  protected def modifiersAST: List[Modifier]
  protected def metaParametersAST: List[MetaParameter]
  protected def superTypeAST: Option[TypeName]
  protected def interfacesAST: List[TypeName]

  protected def implicitModifier: Int
  protected def defaultSuperTypeSignature: JClassTypeSignature
  protected def autoImplementedInterfaceSignatures: List[JClassTypeSignature]
  
  private[ir] def implicitMethodModifier: Int

  lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifier

  lazy val name = internalName.replace('/', '.').replace('$', '.')

  lazy val outerClass: Option[String] = outer.map(_.internalName)
  
  lazy val internalName = outerClass match {
    case Some(outer) => outer + '$' + simpleName
    case None        => file.internalName.map(_ + '/').getOrElse("") + simpleName
  }

  lazy val innerClasses = inners.map(m => m.simpleName -> m.internalName).toMap

  lazy val inners: List[IRClass] = declaredMembers.collect { case m: IRClass => m }

  lazy val fields: List[IRField] = declaredMembers.collect { case f: IRField => f }

  lazy val methods: List[IRMethod] = collectMethods(declaredMembers.collect { case m: IRMethod => m })

  lazy val annotations = file.annotationReader.classAnnotations(modifiersAST.collect { case ann: Annotation => ann })

  lazy val (signature: JClassSignature, resolver: NameResolver) = annotations.signature match {
    case Some(sig) => (sig, sig.metaParams.foldLeft(file.resolver){ _.withMetaParameter(_) }.withInnerClasses(inners))
    case None      => constructResolver(metaParametersAST, Nil, file.resolver)
  }

  lazy val dslInfo = annotations.dsl

  private def constructResolver (metaParametersAST: List[MetaParameter], metaParams: List[FormalMetaParameter], resolver: NameResolver): (JClassSignature, NameResolver) = metaParametersAST match {
    case param :: rest => resolver.metaParameter(param) match {
      case Success(fmp) => constructResolver(rest, fmp :: metaParams, resolver.withMetaParameter(fmp))
      case Failure(e)   =>
        state.error("invalid meta parameter : " + param, e)
        constructResolver(rest, metaParams, resolver)
    }
    case Nil => (constructSignature(metaParams.reverse, resolver), resolver.withInnerClasses(inners))
  }

  private def constructSignature (metaParams: List[FormalMetaParameter], resolver: NameResolver): JClassSignature =
    JClassSignature(metaParams, constructSuperSignature(resolver), constructInterfaceSignatures(resolver))

  private def constructSuperSignature (resolver: NameResolver): JClassTypeSignature = superTypeAST.map { s =>
    state.successOrError(resolver.classTypeSignature(s), "invalid super type of class " + name + " : " + s, defaultSuperTypeSignature)
  }.getOrElse(defaultSuperTypeSignature)

  private def constructInterfaceSignatures (resolver: NameResolver): List[JClassTypeSignature] = constructInterfaceSignatures(interfacesAST, Nil, resolver) ++ autoImplementedInterfaceSignatures

  private def constructInterfaceSignatures (interfaces: List[TypeName], signatures: List[JClassTypeSignature], resolver: NameResolver): List[JClassTypeSignature] = interfaces match {
    case t :: rest => resolver.classTypeSignature(t) match {
      case Success(sig) => constructInterfaceSignatures(rest, sig :: signatures, resolver)
      case Failure(e)   =>
        state.error("invalid interface type name : " + t, e)
        constructInterfaceSignatures(rest, signatures, resolver)
    }
    case Nil => signatures.reverse
  }

  private def collectMethods (methods: List[IRMethod]): List[IRMethod] = methods ++ methods.flatMap(_.paramInitializers)

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

case class IRClassDef (ast: ClassDeclaration, outer: Option[IRClass], file: IRFile) extends IRClass {
  def simpleName: String = ast.name

  lazy val declaredMembers: List[IRMember] = declaredMembers(ast.members, Nil)

  protected def implicitModifier = accSuper
  private[ir] def implicitMethodModifier = 0
  protected def defaultSuperTypeSignature = JTypeSignature.objectTypeSig
  protected def autoImplementedInterfaceSignatures = Nil

  protected def modifiersAST = ast.modifiers
  protected def metaParametersAST = ast.metaParameters
  protected def superTypeAST = ast.superClass
  protected def interfacesAST = ast.interfaces

  private def declaredMembers (membersAST: List[ClassMember], ms: List[IRMember]): List[IRMember] = membersAST match {
    case (m: ModuleDeclaration) :: rest         => declaredMembers(rest, IRClass(m, Some(this), file) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRClassField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IRMethodDef(m, this) :: ms)
    case (c: ConstructorDeclaration) :: rest    => declaredMembers(rest, IRConstructorDef(c, this) :: ms)
    case (i: InstanceInitializer) :: rest       => declaredMembers(rest, IRInstanceInitializer(i, this) :: ms)
    case (s: StaticInitializer) :: rest         => declaredMembers(rest, IRStaticInitializer(s, this) :: ms)
    case Nil => ms.reverse
  }
}

case class IRInterfaceDef (ast: InterfaceDeclaration, outer: Option[IRClass], file: IRFile) extends IRClass {
  def simpleName: String = ast.name

  lazy val declaredMembers: List[IRMember] = declaredMembers(ast.members, Nil)

  protected def implicitModifier = accAbstract | accInterface
  private[ir] def implicitMethodModifier: Int = accPublic | accAbstract
  protected def defaultSuperTypeSignature = JTypeSignature.objectTypeSig
  protected def autoImplementedInterfaceSignatures = Nil

  protected def modifiersAST = ast.modifiers
  protected def metaParametersAST: List[MetaParameter] = ast.metaParameters
  protected def superTypeAST: Option[TypeName] = None
  protected def interfacesAST: List[TypeName] = ast.superInterfaces

  private def declaredMembers (membersAST: List[ClassMember], ms: List[IRMember]): List[IRMember] = membersAST match {
    case (m: ModuleDeclaration) :: rest         => declaredMembers(rest, IRClass(m, Some(this), file) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRInterfaceField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IRMethodDef(m, this) :: ms)
    case (c: ConstructorDeclaration) :: rest    => declaredMembers(rest, IRConstructorDef(c, this) :: ms)
    case (i: InstanceInitializer) :: rest       => declaredMembers(rest, IRInstanceInitializer(i, this) :: ms)
    case (s: StaticInitializer) :: rest         => declaredMembers(rest, IRStaticInitializer(s, this) :: ms)
    case Nil => ms.reverse
  }
}

trait IRField extends JFieldDef with IRMember {
  def declaringClass: IRClass

  protected def modifiersAST: List[Modifier]
  protected def fieldTypeAST: TypeName
  protected def declaratorAST: VariableDeclarator

  protected def implicitModifier: Int

  def name: String = declaratorAST.name

  lazy val signature: JTypeSignature = declaringClass.resolver.typeSignature(fieldTypeAST) match {
    case Success(t) => JTypeSignature.arraySig(t, declaratorAST.dim)
    case Failure(e) => state.errorAndReturn("invalid type of field : " + name, e, JTypeSignature.objectTypeSig)
  }

  lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifier
}

case class IRClassField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRClassDef) extends IRField {
  protected def implicitModifier: Int = 0
}

case class IRInterfaceField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRInterfaceDef) extends IRField {
  protected def implicitModifier: Int = accPublic | accStatic | accFinal
}

trait IRMethod extends JMethodDef with IRMember {
  def declaringClass: IRClass

  protected def modifiersAST: List[Modifier]
  protected def returnTypeAST: Option[TypeName]
  protected def metaParametersAST: List[MetaParameter]
  protected def formalParametersAST: List[FormalParameter]
  protected def clausesAST: List[MethodClause]

  lazy val annotations = declaringClass.file.annotationReader.methodAnnotations(modifiersAST.collect { case ann: Annotation => ann })

  lazy val (signature: JMethodSignature, paramInitializers: List[IRParameterInitializer], resolver: NameResolver) = annotations.signature match {
    case Some(sig) => (sig, Nil, sig.metaParams.foldLeft(declaringClass.resolver){ _.withMetaParameter(_) })
    case None      => constructResolver(metaParametersAST, Nil, declaringClass.resolver)
  }

  private def constructResolver (metaParametersAST: List[MetaParameter], metaParams: List[FormalMetaParameter], resolver: NameResolver): (JMethodSignature, List[IRParameterInitializer], NameResolver) = metaParametersAST match {
    case param :: rest => resolver.metaParameter(param) match {
      case Success(fmp) => constructResolver(rest, fmp :: metaParams, resolver.withMetaParameter(fmp))
      case Failure(e)   =>
        state.error("invalid meta parameter : " + param, e)
        constructResolver(rest, metaParams, resolver)
    }
    case Nil =>
      val formalParameters = formalParametersAST.map(param => new IRFormalParameter(param, resolver, this))
      (constructSignature(metaParams.reverse, formalParameters, resolver), formalParameters.flatMap(_.initializerMethod), resolver)
  }

  private def constructSignature (metaParams: List[FormalMetaParameter], formalParameters: List[IRFormalParameter], resolver: NameResolver): JMethodSignature = {
    JMethodSignature(metaParams, getFormalParameterSignatures(formalParameters, Nil), returnSignature(resolver),
      readClause("throws", { case ThrowsClause(es) => es }, resolver), readClause("activates", { case ActivatesClause(es) => es }, resolver),
      readClause("deactivates", { case DeactivatesClause(es) => es }, resolver), readClause("requires", { case RequiresClause(es) => es }, resolver))
  }

  private def getFormalParameterSignatures (formalParameters: List[IRFormalParameter], signatures: List[JParameterSignature]): List[JParameterSignature] = formalParameters match {
    case param :: rest => param.signature match {
      case Success(sig) => getFormalParameterSignatures(rest, sig :: signatures)
      case Failure(e)   =>
        state.error("invalid formal parameter " + param)
        getFormalParameterSignatures(rest, signatures)
    }
    case Nil => signatures.reverse
  } 

  protected def returnSignature (resolver: NameResolver): JTypeSignature = returnTypeAST.map { t =>
    resolver.typeSignature(t) match {
      case Success(s) => s
      case Failure(e) => state.errorAndReturn("invalid return type : " + t, e, VoidTypeSignature)
    }
  }.getOrElse(VoidTypeSignature)

  private def readClause (name: String, reader: PartialFunction[MethodClause, List[TypeName]], resolver: NameResolver): List[JTypeSignature] = clausesAST.collectFirst(reader).map(readClause(name, _, Nil, resolver)).getOrElse(Nil)

  private def readClause (name: String, types: List[TypeName], signatures: List[JTypeSignature], resolver: NameResolver): List[JTypeSignature] = types match {
    case t :: rest => resolver.typeSignature(t) match {
      case Success(sig) => readClause(name, rest, sig :: signatures, resolver)
      case Failure(e)   =>
        state.error("invalid " + name + " clause of method " + this.name, e)
        readClause(name, rest, signatures, resolver)
    }
    case Nil => signatures.reverse
  }
}

class IRFormalParameter (ast: FormalParameter, resolver: NameResolver, method: IRMethod) {
  private lazy val initializer = ast.initializer.map(snippet => (method.name + "$init$" + method.state.uniqueId, snippet))
  lazy val signature = resolver.parameterSignature(ast, initializer.map(_._1))
  lazy val initializerMethod = for {
    returnType <- signature.map(_.actualTypeSignature).toOption
    (name, snippet) <- initializer
  } yield IRParameterInitializer(method, returnType, name, snippet)
}

case class IRMethodDef (ast: MethodDeclaration, declaringClass: IRClass) extends IRMethod {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers) | declaringClass.implicitMethodModifier

  def name: String = ast.name

  override def syntax: Option[JOperatorSyntaxDef] = None
  
  protected def modifiersAST: List[Modifier] = ast.modifiers
  protected def metaParametersAST: List[MetaParameter] = ast.metaParameters
  protected def returnTypeAST: Option[TypeName] = Some(ast.returnType)
  protected def formalParametersAST: List[FormalParameter] = ast.formalParameters
  protected def clausesAST: List[MethodClause] = ast.clauses
}

case class IRConstructorDef (ast: ConstructorDeclaration, declaringClass: IRClass) extends IRMethod {
  lazy val mod: JModifier = IRModifiers.mod(ast.modifiers)

  def name = CommonNames.constructorName

  override def syntax: Option[JOperatorSyntaxDef] = None

  protected def modifiersAST: List[Modifier] = ast.modifiers
  protected def metaParametersAST: List[MetaParameter] = ast.metaParameters
  protected def returnTypeAST: Option[TypeName] = None
  protected def formalParametersAST: List[FormalParameter] = ast.formalParameters
  protected def clausesAST: List[MethodClause] = ast.clauses
}

trait IRInitializerMethod extends IRMethod {
  protected def modifiersAST: List[Modifier] = Nil
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def formalParametersAST: List[FormalParameter] = Nil
  protected def clausesAST: List[MethodClause] = Nil

  override def syntax: Option[JOperatorSyntaxDef] = None
}

case class IRInstanceInitializer (ast: InstanceInitializer, declaringClass: IRClass) extends IRInitializerMethod {
  def mod: JModifier = JModifier(0)
  def name: String = ""
  protected def returnTypeAST: Option[TypeName] = None
}

case class IRStaticInitializer (ast: StaticInitializer, declaringClass: IRClass) extends IRInitializerMethod {
  def mod: JModifier = JModifier(accStatic)
  def name: String = CommonNames.classInitializerName
  protected def returnTypeAST: Option[TypeName] = None
}

case class IRParameterInitializer (method: IRMethod, returnTypeSignature: JTypeSignature, name: String, snippet: ExpressionSnippet) extends IRInitializerMethod {
  def mod = JModifier(accPublic | accStatic | accFinal)
  def declaringClass: IRClass = method.declaringClass
  protected def returnTypeAST: Option[TypeName] = None
  override protected def returnSignature (resolver: NameResolver): JTypeSignature = returnTypeSignature
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
