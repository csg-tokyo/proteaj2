package phenan.prj.ir

import phenan.prj._
import phenan.prj.JCompiler
import phenan.prj.declaration._

import scala.util._

import JModifier._

case class IRFile (ast: CompilationUnit, root: RootResolver) {
  lazy val modules: List[IRModule] = collectModules(ast.modules.map(IRModule(_, this)), Nil)

  lazy val internalName = ast.header.pack.map(_.name.names.mkString("/"))
  
  private def collectModules (modules: List[IRModule], result: List[IRModule]): List[IRModule] = modules match {
    case m :: ms => collectModules(ms ++ m.inners, result :+ m)
    case Nil     => result
  }

  lazy val resolver = root.file(this)
  def compiler = root.compiler
  def state = compiler.state

  private[ir] def annotationReader = new IRAnnotationReader(this)
}

sealed trait IRMember

trait IRModule extends JClass with IRMember {
  def file: IRFile

  def outer: Option[IRModule]
  def declaredMembers: List[IRMember]

  def simpleName: String
  
  protected def modifiersAST: List[Modifier]
  protected def metaParametersAST: List[MetaParameter]
  protected def superTypeAST: Option[TypeName]
  protected def interfacesAST: List[TypeName]

  protected def implicitModifier: Int
  protected def defaultSuperTypeSignature: JClassTypeSignature
  protected def autoImplementedInterfaceSignatures: List[JClassTypeSignature]

  lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifier

  lazy val name = internalName.replace('/', '.').replace('$', '.')

  lazy val outerClass: Option[String] = outer.map(_.internalName)
  
  lazy val internalName = outerClass match {
    case Some(outer) => outer + '$' + simpleName
    case None        => file.internalName.map(_ + '/').getOrElse("") + simpleName
  }

  lazy val innerClasses = inners.map(m => m.simpleName -> m.internalName).toMap

  lazy val inners: List[IRModule] = declaredMembers.collect { case m: IRModule => m }

  lazy val fields: List[IRField] = declaredMembers.collect { case f: IRField => f }

  lazy val methods: List[IRProcedure] = declaredMembers.collect { case m: IRProcedure => m }

  lazy val parameterInitializer: List[IRParameterInitializer] = methods.flatMap(_.paramInitializers)

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

  def compiler: JCompiler = file.compiler
}

object IRModule {
  def apply (module: ModuleDeclaration, file: IRFile): IRModule = apply(module, None, file)
  def apply (module: ModuleDeclaration, outer: Option[IRModule], file: IRFile): IRModule = module match {
    case c: ClassDeclaration => IRClass(c, outer, file)
    case i: InterfaceDeclaration => IRInterface(i, outer, file)
    case _ => ???
  }
}

case class IRClass (ast: ClassDeclaration, outer: Option[IRModule], file: IRFile) extends IRModule {
  def simpleName: String = ast.name

  lazy val declaredMembers: List[IRMember] = declaredMembers(ast.members, Nil)

  protected def implicitModifier = accSuper
  protected def defaultSuperTypeSignature = JTypeSignature.objectTypeSig
  protected def autoImplementedInterfaceSignatures = Nil

  protected def modifiersAST = ast.modifiers
  protected def metaParametersAST = ast.metaParameters
  protected def superTypeAST = ast.superClass
  protected def interfacesAST = ast.interfaces

  private def declaredMembers (membersAST: List[ClassMember], ms: List[IRMember]): List[IRMember] = membersAST match {
    case (m: ModuleDeclaration) :: rest         => declaredMembers(rest, IRModule(m, Some(this), file) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRClassField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IRClassMethod(m, this) :: ms)
    case (c: ConstructorDeclaration) :: rest    => declaredMembers(rest, IRClassConstructor(c, this) :: ms)
    case (i: InstanceInitializer) :: rest       => declaredMembers(rest, IRInstanceInitializer(i, this) :: ms)
    case (s: StaticInitializer) :: rest         => declaredMembers(rest, IRStaticInitializer(s, this) :: ms)
    case Nil => ms.reverse
  }
}

case class IRInterface (ast: InterfaceDeclaration, outer: Option[IRModule], file: IRFile) extends IRModule {
  def simpleName: String = ast.name

  lazy val declaredMembers: List[IRMember] = declaredMembers(ast.members, Nil)

  protected def implicitModifier = accAbstract | accInterface
  protected def defaultSuperTypeSignature = JTypeSignature.objectTypeSig
  protected def autoImplementedInterfaceSignatures = Nil

  protected def modifiersAST = ast.modifiers
  protected def metaParametersAST: List[MetaParameter] = ast.metaParameters
  protected def superTypeAST: Option[TypeName] = None
  protected def interfacesAST: List[TypeName] = ast.superInterfaces

  private def declaredMembers (membersAST: List[InterfaceMember], ms: List[IRMember]): List[IRMember] = membersAST match {
    case (m: ModuleDeclaration) :: rest         => declaredMembers(rest, IRModule(m, Some(this), file) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRInterfaceField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IRInterfaceMethod(m, this) :: ms)
    case Nil => ms.reverse
  }
}

trait IRField extends JFieldDef with IRMember {
  def declaringClass: IRModule

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

case class IRClassField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRClass) extends IRField {
  protected def implicitModifier: Int = 0
}

case class IRInterfaceField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRInterface) extends IRField {
  protected def implicitModifier: Int = accPublic | accStatic | accFinal
}

trait IRProcedure extends JMethodDef with IRMember {
  def declaringClass: IRModule

  protected def modifiersAST: List[Modifier]
  protected def returnTypeAST: Option[TypeName]
  protected def metaParametersAST: List[MetaParameter]
  protected def formalParametersAST: List[FormalParameter]
  protected def clausesAST: List[MethodClause]

  protected def implicitModifiers: Int

  lazy val mod = IRModifiers.mod(modifiersAST) | implicitModifiers

  lazy val annotations = declaringClass.file.annotationReader.methodAnnotations(modifiersAST.collect { case ann: Annotation => ann })

  lazy val (signature: JMethodSignature, paramInitializers: List[IRParameterInitializer], resolver: NameResolver) = annotations.signature match {
    case Some(sig) => (sig, Nil, sig.metaParams.foldLeft(declaringClass.resolver){ _.withMetaParameter(_) })
    case None      => constructResolver(metaParametersAST, Nil, declaringClass.resolver)
  }

  lazy val syntax = annotations.operator

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

class IRFormalParameter (ast: FormalParameter, resolver: NameResolver, method: IRProcedure) {
  private lazy val initializer = ast.initializer.map(snippet => (method.name + "$init$" + method.state.uniqueId, snippet))
  lazy val signature = resolver.parameterSignature(ast, initializer.map(_._1))
  lazy val initializerMethod = for {
    returnType <- signature.map(_.actualTypeSignature).toOption
    (name, snippet) <- initializer
  } yield IRParameterInitializer(method, returnType, name, snippet)
}

trait IRMethod extends IRProcedure {
  protected def methodAST: MethodDeclaration

  def name: String = methodAST.name

  protected def modifiersAST: List[Modifier] = methodAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = methodAST.metaParameters
  protected def returnTypeAST: Option[TypeName] = Some(methodAST.returnType)
  protected def formalParametersAST: List[FormalParameter] = methodAST.formalParameters
  protected def clausesAST: List[MethodClause] = methodAST.clauses
}

case class IRClassMethod (methodAST: MethodDeclaration, declaringClass: IRClass) extends IRMethod {
  protected def implicitModifiers: Int = 0
}

case class IRInterfaceMethod (methodAST: MethodDeclaration, declaringClass: IRInterface) extends IRMethod {
  protected def implicitModifiers: Int = accPublic | accAbstract
}

trait IRConstructor extends IRProcedure {
  protected def constructorAST: ConstructorDeclaration

  def name: String = CommonNames.constructorName

  protected def modifiersAST: List[Modifier] = constructorAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = constructorAST.metaParameters
  protected def returnTypeAST: Option[TypeName] = None
  protected def formalParametersAST: List[FormalParameter] = constructorAST.formalParameters
  protected def clausesAST: List[MethodClause] = constructorAST.clauses
}

case class IRClassConstructor (constructorAST: ConstructorDeclaration, declaringClass: IRClass) extends IRConstructor {
  protected def implicitModifiers: Int = 0
}

case class IRInstanceInitializer (ast: InstanceInitializer, declaringClass: IRClass) extends IRProcedure {
  def name: String = CommonNames.instanceInitializerName

  protected def modifiersAST: List[Modifier] = Nil
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def returnTypeAST: Option[TypeName] = None
  protected def formalParametersAST: List[FormalParameter] = Nil
  protected def clausesAST: List[MethodClause] = Nil

  protected def implicitModifiers: Int = 0
}

case class IRStaticInitializer (ast: StaticInitializer, declaringClass: IRClass) extends IRProcedure {
  def name: String = CommonNames.classInitializerName

  protected def modifiersAST: List[Modifier] = Nil
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def returnTypeAST: Option[TypeName] = None
  protected def formalParametersAST: List[FormalParameter] = Nil
  protected def clausesAST: List[MethodClause] = Nil

  protected def implicitModifiers: Int = accStatic
}

case class IRParameterInitializer (method: IRProcedure, returnTypeSignature: JTypeSignature, name: String, snippet: ExpressionSnippet) extends JMethodDef {
  def mod: JModifier = JModifier(accPublic | accStatic | accFinal)
  def signature: JMethodSignature = JMethodSignature(Nil, Nil, returnTypeSignature, Nil, Nil, Nil, Nil)
  def syntax: Option[JOperatorSyntaxDef] = None
  def declaringClass: JClass = method.declaringClass

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
