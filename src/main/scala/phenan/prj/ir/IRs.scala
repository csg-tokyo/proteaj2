package phenan.prj.ir

import phenan.prj._
import phenan.prj.JCompiler
import phenan.prj.declaration._

import scala.util._

import JModifier._

case class IRFile (ast: CompilationUnit, root: RootResolver) {
  lazy val modules: List[IRModule] = collectModules(topLevelModules, Nil)

  lazy val topLevelModules = ast.modules.map {
    case c: ClassDeclaration      => IRTopLevelClass(c, this)
    case e: EnumDeclaration       => IRTopLevelEnum(e, this)
    case i: InterfaceDeclaration  => IRTopLevelInterface(i, this)
    case a: AnnotationDeclaration => ???
    case d: DSLDeclaration        => IRTopLevelDSL(d, this)
  }

  lazy val internalName = ast.header.pack.map(_.name.names.mkString("/"))

  def packageName = ast.header.pack.map(_.name)

  def importedClassNames = ast.header.imports.collect { case ClassImportDeclaration(name) => name }

  def importedPackageNames = ast.header.imports.collect { case PackageImportDeclaration(name) => name }
  
  def importedDSLNames = ast.header.imports.collect { case ImportDSLsDeclaration(dsls, _) => dsls }.flatten

  def userConstraints = ast.header.imports.collect { case ImportDSLsDeclaration(_, cs) => cs }.flatten

  lazy val resolver = root.file(this)
  def compiler = root.compiler
  def state = compiler.state

  private def collectModules (modules: List[IRModule], result: List[IRModule]): List[IRModule] = modules match {
    case m :: ms => collectModules(ms ++ m.inners, result :+ m)
    case Nil     => result
  }

  private[ir] def annotationReader = new IRAnnotationReader(this)
}

trait IRMember
sealed trait IRClassMember extends IRMember
sealed trait IREnumMember extends IRMember
sealed trait IRInterfaceMember extends IRMember
sealed trait IRDSLMember extends IRMember
sealed trait IRContextMember extends IRMember

sealed trait IRTopLevelModule extends IRModule {
  def outer: Option[IRModule] = None
}

sealed trait IRInnerModule extends IRModule {
  def declaring: IRModule

  def file = declaring.file
  def outer = Some(declaring)
}

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
  protected def defaultSuperTypeSignature: JClassTypeSignature = JTypeSignature.objectTypeSig
  protected def autoImplementedInterfaceSignatures: List[JClassTypeSignature] = Nil

  protected def priorityNames = dslInfo.map(_.priorities).getOrElse(Nil)

  lazy val declaredPriorities: Set[JPriority] = priorityNames.map(JPriority(SimpleClassTypeSignature(internalName, Nil), _)).toSet

  def memberPriorities: Set[JPriority] = collectMemberPriorities(declaredMembers, Set.empty)

  def priorityConstraints = dslInfo.map(_.constraints).getOrElse(Nil)

  def withDSLs = dslInfo.map(_.withDSLs).getOrElse(Nil)

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

  lazy val methods: List[JMethodDef] = procedures ++ syntheticMethods

  lazy val procedures: List[IRProcedure] = declaredMembers.collect { case m: IRProcedure => m }
  lazy val syntheticMethods: List[IRSyntheticMethod] = procedures.flatMap(_.paramInitializers)

  lazy val annotations = file.annotationReader.read(modifiersAST.collect { case ann: Annotation => ann })

  lazy val (signature: JClassSignature, resolver: NameResolver) = file.annotationReader.classSignature(annotations) match {
    case Some(sig) => (sig, sig.metaParams.foldLeft(file.resolver){ _.withMetaParameter(_) }.withInnerClasses(inners))
    case None      => constructResolver(metaParametersAST, Nil, file.resolver)
  }

  lazy val dslInfo = file.annotationReader.dsl(annotations)

  def compiler: JCompiler = file.compiler

  private def constructResolver (metaParametersAST: List[MetaParameter], metaParams: List[FormalMetaParameter], resolver: NameResolver): (JClassSignature, NameResolver) = metaParametersAST match {
    case param :: rest => resolver.metaParameter(param) match {
      case Success(fmp) => constructResolver(rest, fmp :: metaParams, resolver.withMetaParameter(fmp))
      case Failure(e)   =>
        state.error("invalid meta parameter : " + param, e)
        constructResolver(rest, metaParams, resolver)
    }
    case Nil => (constructSignature(metaParams.reverse, resolver), resolver.withInnerClasses(inners).withPriorities(declaredPriorities))
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

  private def collectMemberPriorities(members: List[IRMember], ps: Set[JPriority]): Set[JPriority] = members match {
    case (o: IROperator) :: rest if o.priority.clazz.internalName == internalName => collectMemberPriorities(rest, ps + o.priority)
    case (m: IRModule) :: rest   => collectMemberPriorities(rest, m.memberPriorities ++ ps)
    case _ :: rest               => collectMemberPriorities(rest, ps)
    case Nil                     => ps
  }
}

trait IRClass extends IRModule {
  protected def classAST: ClassDeclaration

  def simpleName: String = classAST.name
  lazy val declaredMembers: List[IRClassMember] = declaredMembers(classAST.members, Nil)
  
  protected def modifiersAST = classAST.modifiers
  protected def metaParametersAST = classAST.metaParameters
  protected def superTypeAST = classAST.superClass
  protected def interfacesAST = classAST.interfaces

  private def declaredMembers (membersAST: List[ClassMember], ms: List[IRClassMember]): List[IRClassMember] = membersAST match {
    case (c: ClassDeclaration) :: rest          => declaredMembers(rest, IRClassInnerClass(c, this) :: ms)
    case (e: EnumDeclaration) :: rest           => declaredMembers(rest, IRClassInnerEnum(e, this) :: ms)
    case (i: InterfaceDeclaration) :: rest      => declaredMembers(rest, IRClassInnerInterface(i, this) :: ms)
    case (a: AnnotationDeclaration) :: rest     => declaredMembers(rest, ??? :: ms)
    case (d: DSLDeclaration) :: rest            => declaredMembers(rest, IRClassInnerDSL(d, this) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRClassField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IRClassMethod(m, this) :: ms)
    case (c: ConstructorDeclaration) :: rest    => declaredMembers(rest, IRClassConstructor(c, this) :: ms)
    case (i: InstanceInitializer) :: rest       => declaredMembers(rest, IRClassInstanceInitializer(i, this) :: ms)
    case (s: StaticInitializer) :: rest         => declaredMembers(rest, IRClassStaticInitializer(s, this) :: ms)
    case Nil => ms.reverse
  }
}

trait IREnum extends IRModule {
  protected def enumAST: EnumDeclaration

  def simpleName: String = enumAST.name

  lazy val declaredMembers: List[IREnumMember] = enumConstants ++ declaredMembers(enumAST.members, Nil)
  lazy val enumConstants: List[IREnumConstant] = enumAST.enumConstants.map(IREnumConstant(_, this))

  protected def modifiersAST: List[Modifier] = enumAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def superTypeAST: Option[TypeName] = None
  protected def interfacesAST: List[TypeName] = enumAST.interfaces

  override protected def defaultSuperTypeSignature: JClassTypeSignature = JTypeSignature.enumTypeSig(SimpleClassTypeSignature(internalName, Nil))

  private def declaredMembers (membersAST: List[ClassMember], ms: List[IREnumMember]): List[IREnumMember] = membersAST match {
    case (c: ClassDeclaration) :: rest          => declaredMembers(rest, IREnumInnerClass(c, this) :: ms)
    case (e: EnumDeclaration) :: rest           => declaredMembers(rest, IREnumInnerEnum(e, this) :: ms)
    case (i: InterfaceDeclaration) :: rest      => declaredMembers(rest, IREnumInnerInterface(i, this) :: ms)
    case (a: AnnotationDeclaration) :: rest     => declaredMembers(rest, ??? :: ms)
    case (d: DSLDeclaration) :: rest            => declaredMembers(rest, IREnumInnerDSL(d, this) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IREnumField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IREnumMethod(m, this) :: ms)
    case (c: ConstructorDeclaration) :: rest    => declaredMembers(rest, IREnumConstructor(c, this) :: ms)
    case (i: InstanceInitializer) :: rest       => declaredMembers(rest, IREnumInstanceInitializer(i, this) :: ms)
    case (s: StaticInitializer) :: rest         => declaredMembers(rest, IREnumStaticInitializer(s, this) :: ms)
    case Nil => ms.reverse
  }
}

trait IRInterface extends IRModule {
  protected def interfaceAST: InterfaceDeclaration

  def simpleName: String = interfaceAST.name
  lazy val declaredMembers: List[IRInterfaceMember] = declaredMembers(interfaceAST.members, Nil)

  protected def modifiersAST = interfaceAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = interfaceAST.metaParameters
  protected def superTypeAST: Option[TypeName] = None
  protected def interfacesAST: List[TypeName] = interfaceAST.superInterfaces

  private def declaredMembers (membersAST: List[InterfaceMember], ms: List[IRInterfaceMember]): List[IRInterfaceMember] = membersAST match {
    case (c: ClassDeclaration) :: rest          => declaredMembers(rest, IRInterfaceInnerClass(c, this) :: ms)
    case (e: EnumDeclaration) :: rest           => declaredMembers(rest, IRInterfaceInnerEnum(e, this) :: ms)
    case (i: InterfaceDeclaration) :: rest      => declaredMembers(rest, IRInterfaceInnerInterface(i, this) :: ms)
    case (a: AnnotationDeclaration) :: rest     => declaredMembers(rest, ??? :: ms)
    case (d: DSLDeclaration) :: rest            => declaredMembers(rest, IRInterfaceInnerDSL(d, this) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRInterfaceField(mods, ft, _, this)) ++ ms)
    case (m: MethodDeclaration) :: rest         => declaredMembers(rest, IRInterfaceMethod(m, this) :: ms)
    case Nil => ms.reverse
  }
}

trait IRDSL extends IRModule {
  protected def dslAST: DSLDeclaration

  def simpleName: String = dslAST.name
  lazy val declaredMembers: List[IRDSLMember] = declaredMembers(dslAST.members, Nil)

  protected def modifiersAST: List[Modifier] = dslAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def superTypeAST: Option[TypeName] = None
  protected def interfacesAST: List[TypeName] = Nil

  override protected def priorityNames = priorityDeclarations.flatMap(_.priorityNames)

  override lazy val priorityConstraints: List[List[JPriority]] = priorityDeclarations.flatMap(_.constraints)

  override lazy val withDSLs: List[JClassTypeSignature] = withDSLs(dslAST.withDSLs, Nil)

  private def declaredMembers (membersAST: List[DSLMember], ms: List[IRDSLMember]): List[IRDSLMember] = membersAST match {
    case (c: ContextDeclaration) :: rest        => declaredMembers(rest, IRContext(c, this) :: ms)
    case (p: PrioritiesDeclaration) :: rest     => declaredMembers(rest, IRDSLPriorities(p, this) :: ms)
    case (o: OperatorDeclaration) :: rest       => declaredMembers(rest, IRDSLOperator(o, this) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRDSLField(mods, ft, _, this)) ++ ms)
    case Nil => ms.reverse
  }

  private lazy val priorityDeclarations = declaredMembers.collect { case p: IRDSLPriorities => p }

  private def withDSLs (ast: List[QualifiedName], result: List[JClassTypeSignature]): List[JClassTypeSignature] = ast match {
    case qualifiedName :: rest => resolver.classTypeSignature(qualifiedName) match {
      case Success(sig) => withDSLs(rest, sig :: result)
      case Failure(e)   =>
        state.error("invalid DSL name : " + qualifiedName, e)
        withDSLs(rest, result)
    }
    case Nil => result.reverse
  }
}

case class IRContext (contextAST: ContextDeclaration, dsl: IRDSL) extends IRModule with IRDSLMember {
  def file: IRFile = dsl.file
  def outer: Option[IRModule] = Some(dsl)

  def simpleName: String = contextAST.name
  lazy val declaredMembers: List[IRContextMember] = declaredMembers(contextAST.members, Nil)

  protected def modifiersAST: List[Modifier] = contextAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = contextAST.metaParameters
  protected def superTypeAST: Option[TypeName] = None
  protected def interfacesAST: List[TypeName] = Nil

  protected def implicitModifier = accSuper

  private def declaredMembers (membersAST: List[ContextMember], ms: List[IRContextMember]): List[IRContextMember] = membersAST match {
    case (c: ConstructorDeclaration) :: rest    => declaredMembers(rest, IRContextConstructor(c, this) :: ms)
    case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRContextField(mods, ft, _, this)) ++ ms)
    case (o: OperatorDeclaration) :: rest       => declaredMembers(rest, IRContextOperator(o, this) :: ms)
    case Nil => ms.reverse
  }
}

case class IRTopLevelClass (classAST: ClassDeclaration, file: IRFile) extends IRClass with IRTopLevelModule {
  protected def implicitModifier = accSuper
}

case class IRClassInnerClass (classAST: ClassDeclaration, declaring: IRClass) extends IRClass with IRInnerModule with IRClassMember {
  protected def implicitModifier = accSuper
}

case class IREnumInnerClass (classAST: ClassDeclaration, declaring: IREnum) extends IRClass with IRInnerModule with IREnumMember {
  protected def implicitModifier = accSuper
}

case class IRInterfaceInnerClass (classAST: ClassDeclaration, declaring: IRInterface) extends IRClass with IRInnerModule with IRInterfaceMember {
  protected def implicitModifier = accSuper | accStatic | accPublic
}

case class IRTopLevelEnum (enumAST: EnumDeclaration, file: IRFile) extends IREnum with IRTopLevelModule {
  protected def implicitModifier = accSuper
}

case class IRClassInnerEnum (enumAST: EnumDeclaration, declaring: IRClass) extends IREnum with IRInnerModule with IRClassMember {
  protected def implicitModifier = accSuper | accStatic
}

case class IREnumInnerEnum (enumAST: EnumDeclaration, declaring: IREnum) extends IREnum with IRInnerModule with IREnumMember {
  protected def implicitModifier = accSuper | accStatic
}

case class IRInterfaceInnerEnum (enumAST: EnumDeclaration, declaring: IRInterface) extends IREnum with IRInnerModule with IRInterfaceMember {
  protected def implicitModifier = accSuper | accStatic
}

case class IRTopLevelInterface (interfaceAST: InterfaceDeclaration, file: IRFile) extends IRInterface with IRTopLevelModule {
  protected def implicitModifier = accAbstract | accInterface
}

case class IRClassInnerInterface (interfaceAST: InterfaceDeclaration, declaring: IRClass) extends IRInterface with IRInnerModule with IRClassMember {
  protected def implicitModifier = accAbstract | accInterface | accStatic
}

case class IREnumInnerInterface (interfaceAST: InterfaceDeclaration, declaring: IREnum) extends IRInterface with IRInnerModule with IREnumMember {
  protected def implicitModifier = accAbstract | accInterface | accStatic
}

case class IRInterfaceInnerInterface (interfaceAST: InterfaceDeclaration, declaring: IRInterface) extends IRInterface with IRInnerModule with IRInterfaceMember {
  protected def implicitModifier = accAbstract | accInterface | accStatic | accPublic
}

case class IRTopLevelDSL (dslAST: DSLDeclaration, file: IRFile) extends IRDSL with IRTopLevelModule {
  protected def implicitModifier = accSuper
}

case class IRClassInnerDSL (dslAST: DSLDeclaration, declaring: IRClass) extends IRDSL with IRInnerModule with IRClassMember {
  protected def implicitModifier: Int = accSuper | accStatic
}

case class IREnumInnerDSL (dslAST: DSLDeclaration, declaring: IREnum) extends IRDSL with IRInnerModule with IREnumMember {
  protected def implicitModifier: Int = accSuper | accStatic
}

case class IRInterfaceInnerDSL (dslAST: DSLDeclaration, declaring: IRInterface) extends IRDSL with IRInnerModule with IRInterfaceMember {
  protected def implicitModifier: Int = accSuper | accStatic
}

trait IRField extends JFieldDef with IRMember {
  def declaringClass: IRModule

  protected def modifiersAST: List[Modifier]
  protected def implicitModifiers: Int

  lazy val mod = IRModifiers.mod(modifiersAST) | implicitModifiers

  lazy val annotations = declaringClass.file.annotationReader.read(modifiersAST.collect { case ann: Annotation => ann })
}

trait IRMemberVariable extends IRField {
  protected def fieldTypeAST: TypeName
  protected def declaratorAST: VariableDeclarator

  def name: String = declaratorAST.name

  lazy val signature: JTypeSignature = declaringClass.file.annotationReader.fieldSignature(annotations).getOrElse {
    declaringClass.resolver.typeSignature(fieldTypeAST) match {
      case Success(t) => JTypeSignature.arraySig(t, declaratorAST.dim)
      case Failure(e) => state.errorAndReturn("invalid type of field : " + name, e, JTypeSignature.objectTypeSig)
    }
  }
}

case class IRClassField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRClass) extends IRMemberVariable with IRClassMember {
  protected def implicitModifiers = 0
}

case class IREnumField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IREnum) extends IRMemberVariable with IREnumMember {
  protected def implicitModifiers = 0
}

case class IRInterfaceField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRInterface) extends IRMemberVariable with IRInterfaceMember {
  protected def implicitModifiers = accPublic | accStatic | accFinal
}

case class IRDSLField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRDSL) extends IRMemberVariable with IRDSLMember {
  protected def implicitModifiers = accStatic
}

case class IRContextField (modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRContext) extends IRMemberVariable with IRContextMember {
  protected def implicitModifiers = 0
}

case class IREnumConstant (constantAST: EnumConstant, declaringClass: IREnum) extends IRField with IREnumMember {
  protected def modifiersAST: List[Modifier] = constantAST.annotations
  protected def implicitModifiers: Int = accPublic | accStatic | accFinal
  def name: String = constantAST.name
  def signature: JTypeSignature = SimpleClassTypeSignature(declaringClass.internalName, Nil)
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

  lazy val annotations = annReader.read(modifiersAST.collect { case ann: Annotation => ann })

  lazy val (signature: JMethodSignature, paramInitializers: List[IRParameterInitializer], resolver: NameResolver) = annReader.methodSignature(annotations) match {
    case Some(sig) => (sig, Nil, sig.metaParams.foldLeft(declaringClass.resolver){ _.withMetaParameter(_) })
    case None      => constructResolver(metaParametersAST, Nil, declaringClass.resolver)
  }

  lazy val syntax = annReader.operator(annotations)

  private def annReader = declaringClass.file.annotationReader

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

  private def returnSignature (resolver: NameResolver): JTypeSignature = returnTypeAST.map { t =>
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

case class IRClassMethod (methodAST: MethodDeclaration, declaringClass: IRClass) extends IRMethod with IRClassMember {
  protected def implicitModifiers = 0
}

case class IREnumMethod (methodAST: MethodDeclaration, declaringClass: IREnum) extends IRMethod with IREnumMember {
  protected def implicitModifiers = 0
}

case class IRInterfaceMethod (methodAST: MethodDeclaration, declaringClass: IRInterface) extends IRMethod with IRInterfaceMember {
  protected def implicitModifiers = accPublic | accAbstract
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

case class IRClassConstructor (constructorAST: ConstructorDeclaration, declaringClass: IRClass) extends IRConstructor with IRClassMember {
  protected def implicitModifiers = 0
}

case class IREnumConstructor (constructorAST: ConstructorDeclaration, declaringClass: IREnum) extends IRConstructor with IREnumMember {
  protected def implicitModifiers = 0
}

case class IRContextConstructor (constructorAST: ConstructorDeclaration, declaringClass: IRContext) extends IRConstructor with IRContextMember {
  protected def implicitModifiers = 0
}

trait IRInstanceInitializer extends IRProcedure {
  protected def instanceInitializerAST: InstanceInitializer

  def name: String = CommonNames.instanceInitializerName

  protected def modifiersAST: List[Modifier] = Nil
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def returnTypeAST: Option[TypeName] = None
  protected def formalParametersAST: List[FormalParameter] = Nil
  protected def clausesAST: List[MethodClause] = Nil

  protected def implicitModifiers: Int = 0
}

case class IRClassInstanceInitializer (instanceInitializerAST: InstanceInitializer, declaringClass: IRClass) extends IRInstanceInitializer with IRClassMember

case class IREnumInstanceInitializer (instanceInitializerAST: InstanceInitializer, declaringClass: IREnum) extends IRInstanceInitializer with IREnumMember

trait IRStaticInitializer extends IRProcedure {
  protected def staticInitializerAST: StaticInitializer

  def name: String = CommonNames.classInitializerName

  protected def modifiersAST: List[Modifier] = Nil
  protected def metaParametersAST: List[MetaParameter] = Nil
  protected def returnTypeAST: Option[TypeName] = None
  protected def formalParametersAST: List[FormalParameter] = Nil
  protected def clausesAST: List[MethodClause] = Nil

  protected def implicitModifiers: Int = accStatic
}

case class IRClassStaticInitializer (staticInitializerAST: StaticInitializer, declaringClass: IRClass) extends IRStaticInitializer with IRClassMember

case class IREnumStaticInitializer (staticInitializerAST: StaticInitializer, declaringClass: IREnum) extends IRStaticInitializer with IREnumMember

trait IROperator extends IRProcedure {
  def dsl: IRDSL
  protected def operatorAST: OperatorDeclaration

  lazy val name: String = operatorAST.label.getOrElse("ProteanOperator$" + state.uniqueId)

  protected def modifiersAST: List[Modifier] = operatorAST.modifiers
  protected def metaParametersAST: List[MetaParameter] = operatorAST.metaParameters
  protected def returnTypeAST: Option[TypeName] = Some(operatorAST.returnType)
  protected def formalParametersAST: List[FormalParameter] = operatorAST.formalParameters
  protected def clausesAST: List[MethodClause] = operatorAST.clauses

  override lazy val syntax = Some {
    if (modifiersAST.contains(LiteralModifier)) JLiteralSyntaxDef(priority, pattern)
    else JExpressionSyntaxDef(priority, pattern)
  }

  lazy val priority: JPriority = operatorAST.priority.flatMap(resolver.priority).getOrElse {
    JPriority(SimpleClassTypeSignature(dsl.internalName, Nil), "ProteanOperatorPriority$" + state.uniqueId)
  }

  lazy val pattern = operatorAST.syntax map {
    case OperatorName(n) => JOperatorNameDef(n)
    case MetaValueRef(n) => JMetaValueRefDef(n)
    case Operand         => JOperandDef
    case Repetition0     => JRepetition0Def
    case Repetition1     => JRepetition1Def
    case OptionalOperand => JOptionalOperandDef
    case AndPredicate(t, p) => JAndPredicateDef(predicateSignature(t, p))
    case NotPredicate(t, p) => JNotPredicateDef(predicateSignature(t, p))
  }

  private def predicateSignature (t: TypeName, p: Option[QualifiedName]): JParameterSignature = {
    val sig = state.successOrError(resolver.typeSignature(t), "invalid predicate type : " + t, VoidTypeSignature)
    JParameterSignature(Nil, sig, p.flatMap(resolver.priority), false, None)
  }
}

case class IRDSLOperator (operatorAST: OperatorDeclaration, declaringClass: IRDSL) extends IROperator with IRDSLMember {
  def dsl: IRDSL = declaringClass
  protected def implicitModifiers: Int = accStatic | accFinal
}

case class IRContextOperator (operatorAST: OperatorDeclaration, declaringClass: IRContext) extends IROperator with IRContextMember {
  def dsl: IRDSL = declaringClass.dsl
  protected def implicitModifiers = 0
}

case class IRDSLPriorities (prioritiesAST: PrioritiesDeclaration, declaringDSL: IRDSL) extends IRDSLMember {
  def priorityNames = prioritiesAST.names

  lazy val constraints: List[List[JPriority]] = prioritiesAST.constraints.map(declaringDSL.resolver.constraint)
}


sealed trait IRSyntheticMethod extends JMethodDef with IRMember {
  def mod: JModifier = JModifier(modifiers)
  def syntax: Option[JSyntaxDef] = None
  def signature: JMethodSignature = JMethodSignature(Nil, parameterSignatures, returnTypeSignature, Nil, Nil, Nil, Nil)
  
  protected def modifiers: Int
  protected def parameterSignatures: List[JParameterSignature]
  protected def returnTypeSignature: JTypeSignature
}

case class IRParameterInitializer (method: IRProcedure, returnTypeSignature: JTypeSignature, name: String, snippet: ExpressionSnippet) extends IRSyntheticMethod {
  protected def modifiers = accPublic | accStatic | accFinal
  protected def parameterSignatures: List[JParameterSignature] = Nil
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

sealed trait IRAnnotationElement

case class IRAnnotation (annotationClass: JClass, args: Map[String, IRAnnotationElement]) extends IRAnnotationElement

case class IRAnnotationElementArray (array: List[IRAnnotationElement]) extends IRAnnotationElement

case class IRAnnotationElementString (str: String) extends IRAnnotationElement

case class IRAnnotationElementClass (signature: JTypeSignature) extends IRAnnotationElement

case class IRAnnotationElementEnumConstant (clazz: Option[JClass], name: String) extends IRAnnotationElement