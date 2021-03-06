package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._
import phenan.prj.body.BodyCompiler

import scala.util._
import JModifier._
import phenan.prj.exception._

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait IRs {
  this: BodyCompiler with IRStatements with IRExpressions with NameResolvers with Environments with FileEnvironments with IRAnnotationReader with JTypeLoader with JModules with JErasedTypes with Application =>

  case class IRFile (ast: CompilationUnit, filePath: String) {
    lazy val modules: List[IRModule] = collectModules(topLevelModules, Nil)

    lazy val topLevelModules: List[IRTopLevelModule] = ast.modules.map {
      case c: ClassDeclaration => IRTopLevelClass(c, this)
      case e: EnumDeclaration => IRTopLevelEnum(e, this)
      case i: InterfaceDeclaration => IRTopLevelInterface(i, this)
      case _: AnnotationDeclaration => ???
      case d: DSLDeclaration => IRTopLevelDSL(d, this)
    }

    lazy val internalName: Option[String] = ast.header.pack.map(_.name.names.mkString("/"))

    def packageName: Option[QualifiedName] = ast.header.pack.map(_.name)

    def importedClassNames: List[QualifiedName] = ast.header.imports.collect { case ClassImportDeclaration(name) => name }

    def importedPackageNames: List[QualifiedName] = ast.header.imports.collect { case PackageImportDeclaration(name) => name }

    def importedDSLNames: List[QualifiedName] = ast.header.imports.collect { case ImportDSLsDeclaration(dsls, _) => dsls }.flatten

    def userConstraints: List[List[QualifiedName]] = ast.header.imports.collect { case ImportDSLsDeclaration(_, cs) => cs }.flatten

    lazy val environment = FileEnvironment(this)

    lazy val resolver: NameResolver = rootResolver.file(this)

    private def collectModules(modules: List[IRModule], result: List[IRModule]): List[IRModule] = modules match {
      case m :: ms => collectModules(ms ++ m.inners, result :+ m)
      case Nil => result
    }
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

    def file: IRFile = declaring.file

    def outer = Some(declaring)
  }

  trait IRModule extends JClass with IRMember with IRAnnotationReaders {
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

    protected def priorityNames: List[String] = dslInfo.map(_.priorities).getOrElse(Nil)

    lazy val declaredPriorities: Set[JPriority] = priorityNames.map(JPriority(SimpleClassTypeSignature(internalName, Nil), _)).toSet

    def memberPriorities: Set[JPriority] = collectMemberPriorities(declaredMembers, Set.empty)

    def priorityConstraints: List[List[JPriority]] = dslInfo.map(_.constraints).getOrElse(Nil)

    def withDSLs: List[JClass] = dslInfo.map(_.withDSLs).getOrElse(Nil)

    lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifier

    lazy val name: String = internalName.replace('/', '.').replace('$', '.')

    lazy val outerClass: Option[String] = outer.map(_.internalName)

    lazy val internalName: String = outerClass match {
      case Some(outer) => outer + '$' + simpleName
      case None => file.internalName.map(_ + '/').getOrElse("") + simpleName
    }

    lazy val innerClasses: Map[String, String] = inners.map(m => m.simpleName -> m.internalName).toMap

    lazy val inners: List[IRModule] = declaredMembers.collect { case m: IRModule => m }

    lazy val fields: List[JFieldDef] = declaredMembers.collect { case f: JFieldDef => f }

    lazy val methods: List[JMethodDef] = procedures ++ syntheticMethods

    lazy val procedures: List[IRProcedure] = declaredMembers.collect { case m: IRProcedure => m }
    lazy val syntheticMethods: List[IRSyntheticMethod] = {
      fields.collect { case f: IRField => f }.flatMap(_.initializer) ++ procedures.flatMap(_.paramInitializers)
    }

    lazy val annotations: List[IRAnnotation] = readAnnotations(modifiersAST.collect { case ann: Annotation => ann })

    lazy val staticResolver: NameResolver = getInitialResolver.withInnerClasses(inners).withPriorities(declaredPriorities)

    private def getInitialResolver: NameResolver = outer match {
      case Some(m) =>
        if (mod.check(JModifier.accStatic)) m.staticResolver
        else m.resolver
      case None => file.resolver
    }

    lazy val (signature: JClassSignature, resolver: NameResolver) = readClassSignature(annotations) match {
      case Some(sig) => (sig, sig.metaParams.foldLeft(staticResolver) {
        _.withMetaParameter(_)
      })
      case None => constructResolver(metaParametersAST, Nil, staticResolver)
    }

    lazy val thisType: JObjectType = metaParametersRef.flatMap(getObjectType(this, _)) match {
      case Success(t) => t
      case Failure(e) => throw InvalidTypeException(s"this object of class $name has invalid type", e)
    }

    lazy val metaParameters: MetaArgs = metaParametersRef.map(args => signature.metaParams.map(_.name).zip(args).toMap) match {
      case Success(args) => args
      case Failure(e) => throw InvalidTypeException(s"invalid meta parameters for class $name : $metaParametersRef", e)
    }

    lazy val staticEnvironment: ModuleEnvironment = file.environment.staticEnvironment(this)
    lazy val instanceEnvironment: ModuleEnvironment = file.environment.instanceEnvironment(this)

    def isDSL: Boolean = dslInfo.nonEmpty

    /* */

    private lazy val dslInfo = readDsl(annotations)

    private lazy val metaParametersRef: Try[List[MetaArgument]] = metaParametersRef(signature.metaParams, Nil)

    private def metaParametersRef(mps: List[FormalMetaParameter], ref: List[MetaArgument]): Try[List[MetaArgument]] = mps match {
      case mp :: rest => resolver.metaArgumentFor(mp.name) match {
        case Success(arg) => metaParametersRef(rest, arg :: ref)
        case Failure(e)   => Failure(e)
      }
      case Nil => Success(ref.reverse)
    }

    private def constructResolver(metaParametersAST: List[MetaParameter], metaParams: List[FormalMetaParameter], resolver: NameResolver): (JClassSignature, NameResolver) = metaParametersAST match {
      case param :: rest => resolver.metaParameter(param) match {
        case Success(fmp) => constructResolver(rest, fmp :: metaParams, resolver.withMetaParameter(fmp))
        case Failure(e) =>
          error("invalid meta parameter : " + param, e)
          constructResolver(rest, metaParams, resolver)
      }
      case Nil => (constructSignature(metaParams.reverse, resolver), resolver)
    }

    private def constructSignature(metaParams: List[FormalMetaParameter], resolver: NameResolver): JClassSignature =
      JClassSignature(metaParams, constructSuperSignature(resolver), constructInterfaceSignatures(resolver))

    private def constructSuperSignature(resolver: NameResolver): JClassTypeSignature = superTypeAST.map { s =>
      successOrError(resolver.classTypeSignature(s), "invalid super type of class " + name + " : " + s, defaultSuperTypeSignature)
    }.getOrElse(defaultSuperTypeSignature)

    private def constructInterfaceSignatures(resolver: NameResolver): List[JClassTypeSignature] = constructInterfaceSignatures(interfacesAST, Nil, resolver) ++ autoImplementedInterfaceSignatures

    private def constructInterfaceSignatures(interfaces: List[TypeName], signatures: List[JClassTypeSignature], resolver: NameResolver): List[JClassTypeSignature] = interfaces match {
      case t :: rest => resolver.classTypeSignature(t) match {
        case Success(sig) => constructInterfaceSignatures(rest, sig :: signatures, resolver)
        case Failure(e) =>
          error("invalid interface type name : " + t, e)
          constructInterfaceSignatures(rest, signatures, resolver)
      }
      case Nil => signatures.reverse
    }

    private def collectMemberPriorities(members: List[IRMember], ps: Set[JPriority]): Set[JPriority] = members match {
      case (o: IROperator) :: rest => collectMemberPriorities(rest, ps + o.priority)
      case (m: IRModule) :: rest => collectMemberPriorities(rest, m.memberPriorities ++ ps)
      case _ :: rest => collectMemberPriorities(rest, ps)
      case Nil => ps
    }
  }

  trait IRClass extends IRModule {
    protected def classAST: ClassDeclaration

    def simpleName: String = classAST.name

    lazy val declaredMembers: List[IRClassMember] = declaredMembers(classAST.members, Nil)

    protected def modifiersAST: List[Modifier] = classAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = classAST.metaParameters

    protected def superTypeAST: Option[TypeName] = classAST.superClass

    protected def interfacesAST: List[TypeName] = classAST.interfaces

    private def declaredMembers(membersAST: List[ClassMember], ms: List[IRClassMember]): List[IRClassMember] = membersAST match {
      case (c: ClassDeclaration) :: rest => declaredMembers(rest, IRClassInnerClass(c, this) :: ms)
      case (e: EnumDeclaration) :: rest => declaredMembers(rest, IRClassInnerEnum(e, this) :: ms)
      case (i: InterfaceDeclaration) :: rest => declaredMembers(rest, IRClassInnerInterface(i, this) :: ms)
      case (_: AnnotationDeclaration) :: rest => declaredMembers(rest, ??? :: ms)
      case (d: DSLDeclaration) :: rest => declaredMembers(rest, IRClassInnerDSL(d, this) :: ms)
      case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRClassField(mods, ft, _, this)) ++ ms)
      case (m: MethodDeclaration) :: rest => declaredMembers(rest, IRClassMethod(m, this) :: ms)
      case (c: ConstructorDeclaration) :: rest => declaredMembers(rest, IRClassConstructor(c, this) :: ms)
      case (i: InstanceInitializer) :: rest => declaredMembers(rest, IRClassInstanceInitializer(i, this) :: ms)
      case (s: StaticInitializer) :: rest => declaredMembers(rest, IRClassStaticInitializer(s, this) :: ms)
      case Nil => ms.reverse
    }
  }

  trait IREnum extends IRModule {
    protected def enumAST: EnumDeclaration

    def simpleName: String = enumAST.name

    def declaredMembers: List[IRMember] = enumConstants ++ enumMembers

    lazy val enumConstants: List[IREnumConstant] = enumAST.enumConstants.map(IREnumConstant(_, this))
    lazy val enumMembers: List[IREnumMember] = declaredMembers(enumAST.members, Nil)

    protected def modifiersAST: List[Modifier] = enumAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = Nil

    protected def superTypeAST: Option[TypeName] = None

    protected def interfacesAST: List[TypeName] = enumAST.interfaces

    override protected def defaultSuperTypeSignature: JClassTypeSignature = JTypeSignature.enumTypeSig(SimpleClassTypeSignature(internalName, Nil))

    private def declaredMembers(membersAST: List[ClassMember], ms: List[IREnumMember]): List[IREnumMember] = membersAST match {
      case (c: ClassDeclaration) :: rest => declaredMembers(rest, IREnumInnerClass(c, this) :: ms)
      case (e: EnumDeclaration) :: rest => declaredMembers(rest, IREnumInnerEnum(e, this) :: ms)
      case (i: InterfaceDeclaration) :: rest => declaredMembers(rest, IREnumInnerInterface(i, this) :: ms)
      case (_: AnnotationDeclaration) :: rest => declaredMembers(rest, ??? :: ms)
      case (d: DSLDeclaration) :: rest => declaredMembers(rest, IREnumInnerDSL(d, this) :: ms)
      case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IREnumField(mods, ft, _, this)) ++ ms)
      case (m: MethodDeclaration) :: rest => declaredMembers(rest, IREnumMethod(m, this) :: ms)
      case (c: ConstructorDeclaration) :: rest => declaredMembers(rest, IREnumConstructor(c, this) :: ms)
      case (i: InstanceInitializer) :: rest => declaredMembers(rest, IREnumInstanceInitializer(i, this) :: ms)
      case (s: StaticInitializer) :: rest => declaredMembers(rest, IREnumStaticInitializer(s, this) :: ms)
      case Nil => ms.reverse
    }
  }

  trait IRInterface extends IRModule {
    protected def interfaceAST: InterfaceDeclaration

    def simpleName: String = interfaceAST.name

    lazy val declaredMembers: List[IRInterfaceMember] = declaredMembers(interfaceAST.members, Nil)

    protected def modifiersAST: List[Modifier] = interfaceAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = interfaceAST.metaParameters

    protected def superTypeAST: Option[TypeName] = None

    protected def interfacesAST: List[TypeName] = interfaceAST.superInterfaces

    private def declaredMembers(membersAST: List[InterfaceMember], ms: List[IRInterfaceMember]): List[IRInterfaceMember] = membersAST match {
      case (c: ClassDeclaration) :: rest => declaredMembers(rest, IRInterfaceInnerClass(c, this) :: ms)
      case (e: EnumDeclaration) :: rest => declaredMembers(rest, IRInterfaceInnerEnum(e, this) :: ms)
      case (i: InterfaceDeclaration) :: rest => declaredMembers(rest, IRInterfaceInnerInterface(i, this) :: ms)
      case (_: AnnotationDeclaration) :: rest => declaredMembers(rest, ??? :: ms)
      case (d: DSLDeclaration) :: rest => declaredMembers(rest, IRInterfaceInnerDSL(d, this) :: ms)
      case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRInterfaceField(mods, ft, _, this)) ++ ms)
      case (m: MethodDeclaration) :: rest => declaredMembers(rest, IRInterfaceMethod(m, this) :: ms)
      case Nil => ms.reverse
    }
  }

  trait IRDSL extends IRModule {
    protected def dslAST: DSLDeclaration

    def simpleName: String = dslAST.name

    lazy val (declaredMembers: List[IRDSLMember], priorityDeclarations: List[IRPriorities]) = declaredMembers(dslAST.members, Nil, Nil)

    protected def modifiersAST: List[Modifier] = dslAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = dslAST.metaParameters

    protected def superTypeAST: Option[TypeName] = None

    protected def interfacesAST: List[TypeName] = Nil

    override protected def priorityNames: List[String] = priorityDeclarations.flatMap(_.priorityNames)

    override lazy val priorityConstraints: List[List[JPriority]] = priorityDeclarations.flatMap(_.constraints)

    override lazy val withDSLs: List[JClass] = withDSLs(dslAST.withDSLs, Nil)

    override def isDSL: Boolean = true

    private def declaredMembers(membersAST: List[DSLMember], ms: List[IRDSLMember], ps: List[IRPriorities]): (List[IRDSLMember], List[IRPriorities]) = membersAST match {
      case (p: PrioritiesDeclaration) :: rest => declaredMembers(rest, ms, IRPriorities(p, this) :: ps)
      case (o: OperatorDeclaration) :: rest => declaredMembers(rest, IROperator(o, this) :: ms, ps)
      case (c: ConstructorDeclaration) :: rest => declaredMembers(rest, IRDSLConstructor(c, this) :: ms, ps)
      case FieldDeclaration(mods, ft, ds) :: rest => declaredMembers(rest, ds.map(IRDSLField(mods, ft, _, this)) ++ ms, ps)
      case Nil => (ms.reverse, ps.reverse)
    }

    private def withDSLs(ast: List[QualifiedName], result: List[JClass]): List[JClass] = ast match {
      case qualifiedName :: rest => staticResolver.resolve(qualifiedName.names) match {
        case Success(sig) => withDSLs(rest, sig :: result)
        case Failure(e) =>
          error("invalid DSL name : " + qualifiedName, e)
          withDSLs(rest, result)
      }
      case Nil => result.reverse
    }
  }

  case class IRTopLevelClass(classAST: ClassDeclaration, file: IRFile) extends IRClass with IRTopLevelModule {
    protected def implicitModifier: Int = accSuper
  }

  case class IRClassInnerClass(classAST: ClassDeclaration, declaring: IRClass) extends IRClass with IRInnerModule with IRClassMember {
    protected def implicitModifier: Int = accSuper
  }

  case class IREnumInnerClass(classAST: ClassDeclaration, declaring: IREnum) extends IRClass with IRInnerModule with IREnumMember {
    protected def implicitModifier: Int = accSuper
  }

  case class IRInterfaceInnerClass(classAST: ClassDeclaration, declaring: IRInterface) extends IRClass with IRInnerModule with IRInterfaceMember {
    protected def implicitModifier: Int = accSuper | accStatic | accPublic
  }

  case class IRTopLevelEnum(enumAST: EnumDeclaration, file: IRFile) extends IREnum with IRTopLevelModule {
    protected def implicitModifier: Int = accSuper
  }

  case class IRClassInnerEnum(enumAST: EnumDeclaration, declaring: IRClass) extends IREnum with IRInnerModule with IRClassMember {
    protected def implicitModifier: Int = accSuper | accStatic
  }

  case class IREnumInnerEnum(enumAST: EnumDeclaration, declaring: IREnum) extends IREnum with IRInnerModule with IREnumMember {
    protected def implicitModifier: Int = accSuper | accStatic
  }

  case class IRInterfaceInnerEnum(enumAST: EnumDeclaration, declaring: IRInterface) extends IREnum with IRInnerModule with IRInterfaceMember {
    protected def implicitModifier: Int = accSuper | accStatic
  }

  case class IRTopLevelInterface(interfaceAST: InterfaceDeclaration, file: IRFile) extends IRInterface with IRTopLevelModule {
    protected def implicitModifier: Int = accAbstract | accInterface
  }

  case class IRClassInnerInterface(interfaceAST: InterfaceDeclaration, declaring: IRClass) extends IRInterface with IRInnerModule with IRClassMember {
    protected def implicitModifier: Int = accAbstract | accInterface | accStatic
  }

  case class IREnumInnerInterface(interfaceAST: InterfaceDeclaration, declaring: IREnum) extends IRInterface with IRInnerModule with IREnumMember {
    protected def implicitModifier: Int = accAbstract | accInterface | accStatic
  }

  case class IRInterfaceInnerInterface(interfaceAST: InterfaceDeclaration, declaring: IRInterface) extends IRInterface with IRInnerModule with IRInterfaceMember {
    protected def implicitModifier: Int = accAbstract | accInterface | accStatic | accPublic
  }

  case class IRTopLevelDSL(dslAST: DSLDeclaration, file: IRFile) extends IRDSL with IRTopLevelModule {
    protected def implicitModifier: Int = accSuper
  }

  case class IRClassInnerDSL(dslAST: DSLDeclaration, declaring: IRClass) extends IRDSL with IRInnerModule with IRClassMember {
    protected def implicitModifier: Int = accSuper | accStatic
  }

  case class IREnumInnerDSL(dslAST: DSLDeclaration, declaring: IREnum) extends IRDSL with IRInnerModule with IREnumMember {
    protected def implicitModifier: Int = accSuper | accStatic
  }

  case class IRInterfaceInnerDSL(dslAST: DSLDeclaration, declaring: IRInterface) extends IRDSL with IRInnerModule with IRInterfaceMember {
    protected def implicitModifier: Int = accSuper | accStatic
  }

  trait IRField extends JFieldDef with IRMember with IRAnnotationReaders {
    def declaringClass: IRModule

    protected def modifiersAST: List[Modifier]

    protected def implicitModifiers: Int

    protected def fieldTypeAST: TypeName

    protected def declaratorAST: VariableDeclarator

    def file: IRFile = declaringClass.file

    lazy val annotations: List[IRAnnotation] = readAnnotations(modifiersAST.collect { case ann: Annotation => ann })

    lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifiers

    def name: String = declaratorAST.name

    lazy val signature: JTypeSignature = readFieldSignature(annotations).getOrElse {
      resolver.typeSignature(fieldTypeAST) match {
        case Success(t) => JTypeSignature.arraySig(t, declaratorAST.dim)
        case Failure(e) => errorAndReturn("invalid type of field : " + name, e, JTypeSignature.objectTypeSig)
      }
    }

    def resolver: NameResolver = {
      if (isStatic) declaringClass.staticResolver
      else declaringClass.resolver
    }

    lazy val initializer: Option[IRFieldInitializer] = declaratorAST.initializer.map(IRFieldInitializer(this, _))
  }

  case class IRClassField(modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRClass) extends IRField with IRClassMember {
    protected def implicitModifiers = 0
  }

  case class IREnumField(modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IREnum) extends IRField with IREnumMember {
    protected def implicitModifiers = 0
  }

  case class IRInterfaceField(modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRInterface) extends IRField with IRInterfaceMember {
    protected def implicitModifiers: Int = accPublic | accStatic | accFinal
  }

  case class IRDSLField(modifiersAST: List[Modifier], fieldTypeAST: TypeName, declaratorAST: VariableDeclarator, declaringClass: IRDSL) extends IRField with IRDSLMember {
    protected def implicitModifiers = 0
  }

  case class IREnumConstant(constantAST: EnumConstant, declaringClass: IREnum) extends JFieldDef with IRMember with IRAnnotationReaders {
    protected def modifiersAST: List[Modifier] = constantAST.annotations

    protected def implicitModifiers: Int = accPublic | accStatic | accFinal

    lazy val annotations: List[IRAnnotation] = readAnnotations(modifiersAST.collect { case ann: Annotation => ann })

    lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifiers

    def name: String = constantAST.name

    def signature: JTypeSignature = SimpleClassTypeSignature(declaringClass.internalName, Nil)

    def file: IRFile = declaringClass.file
  }

  trait IRProcedure extends JMethodDef with IRMember with IRAnnotationReaders {

    def declaringClass: IRModule

    protected def modifiersAST: List[Modifier]

    protected def returnTypeAST: Option[TypeName]

    protected def returnBoundsAST: List[TypeName]

    protected def metaParametersAST: List[MetaParameter]

    protected def formalParametersAST: List[FormalParameter]

    protected def clausesAST: List[MethodClause]

    protected def implicitModifiers: Int

    override def file: IRFile = declaringClass.file

    lazy val mod: JModifier = IRModifiers.mod(modifiersAST) | implicitModifiers

    lazy val annotations: List[IRAnnotation] = readAnnotations(modifiersAST.collect { case ann: Annotation => ann })

    lazy val (signature: JMethodSignature, parameters: List[IRFormalParameter], resolver: NameResolver) = readMethodSignature(annotations) match {
      case Some(sig) => fromSignature(sig, getResolver)
      case None => constructResolver(metaParametersAST, Nil, getResolver)
    }

    def syntax: Option[JSyntaxDef] = methodSyntax

    def paramInitializers: List[IRParameterInitializer] = parameters.flatMap(_.initializerMethod)

    def parameterVariables: List[(JType, String)] = parametersRef(parameters, Nil, metaArguments).getOrElse {
      error("invalid parameter type")
      Nil
    }

    lazy val activateTypes: List[JObjectType] = signature.activates.flatMap(fromClassTypeSignature(_, metaArguments))

    lazy val requiresContexts: List[IRContextRef] = {
      someOrError(signature.requires.traverse(fromTypeSignature(_, metaArguments).collect { case obj: JObjectType => IRContextRef(obj) }), "invalid required context type", Nil)
    }

    lazy val exceptions: List[JRefType] = signature.throwTypes.flatMap(fromTypeSignature_RefType(_, metaArguments))

    def environment: ProcedureEnvironment = {
      if (isStatic) declaringClass.staticEnvironment.procedureEnvironment(this)
      else declaringClass.instanceEnvironment.procedureEnvironment(this)
    }

    lazy val metaArguments: MetaArgs = {
      if (isStatic) metaArguments(signature.metaParams, Map.empty)
      else metaArguments(signature.metaParams, declaringClass.metaParameters)
    }

    private lazy val methodSyntax = readOperator(annotations)

    private def getResolver = {
      if (isStatic) declaringClass.staticResolver
      else declaringClass.resolver
    }

    private def parametersRef(params: List[IRFormalParameter], ref: List[(JType, String)], bind: Map[String, MetaArgument]): Option[List[(JType, String)]] = params match {
      case param :: rest => param.actualTypeSignature.flatMap(fromTypeSignature(_, bind)) match {
        case Some(t) => parametersRef(rest, (t, param.name) :: ref, bind)
        case None => None
      }
      case Nil => Some(ref.reverse)
    }

    private def metaArguments(mps: List[FormalMetaParameter], bind: Map[String, MetaArgument]): Map[String, MetaArgument] = mps match {
      case mp :: rest => resolver.metaArgumentFor(mp.name) match {
        case Success(arg) => metaArguments(rest, bind + (mp.name -> arg))
        case Failure(e) =>
          error("invalid meta parameters : " + mp.name, e)
          metaArguments(rest, bind)
      }
      case Nil => bind
    }

    private def fromSignature(sig: JMethodSignature, resolver: NameResolver): (JMethodSignature, List[IRFormalParameter], NameResolver) = {
      val r = sig.metaParams.foldLeft(resolver) {
        _.withMetaParameter(_)
      }
      (sig, formalParametersAST.map(new IRFormalParameter(_, r, this)), r)
    }

    private def constructResolver(metaParametersAST: List[MetaParameter], metaParams: List[FormalMetaParameter], resolver: NameResolver): (JMethodSignature, List[IRFormalParameter], NameResolver) = metaParametersAST match {
      case param :: rest => resolver.metaParameter(param) match {
        case Success(fmp) => constructResolver(rest, fmp :: metaParams, resolver.withMetaParameter(fmp))
        case Failure(e) =>
          error("invalid meta parameter : " + param, e)
          constructResolver(rest, metaParams, resolver)
      }
      case Nil =>
        val formalParameters = formalParametersAST.map(param => new IRFormalParameter(param, resolver, this))
        (constructSignature(metaParams.reverse, formalParameters, resolver), formalParameters, resolver)
    }

    private def constructSignature(metaParams: List[FormalMetaParameter], formalParameters: List[IRFormalParameter], resolver: NameResolver): JMethodSignature = {
      JMethodSignature(metaParams, getFormalParameterSignatures(formalParameters, Nil), returnSignature(resolver), returnBoundSignatures(resolver),
        readClause("throws", { case ThrowsClause(es) => es }, resolver), readClause("activates", { case ActivatesClause(es) => es }, resolver),
        readClause("deactivates", { case DeactivatesClause(es) => es }, resolver), readClause("requires", { case RequiresClause(es) => es }, resolver))
    }

    private def getFormalParameterSignatures(formalParameters: List[IRFormalParameter], signatures: List[JParameterSignature]): List[JParameterSignature] = formalParameters match {
      case param :: rest => param.signature match {
        case Success(sig) => getFormalParameterSignatures(rest, sig :: signatures)
        case Failure(_) =>
          error("invalid formal parameter " + param)
          getFormalParameterSignatures(rest, signatures)
      }
      case Nil => signatures.reverse
    }

    private def returnSignature(resolver: NameResolver): JTypeSignature = returnTypeAST.map { t =>
      resolver.typeSignature(t) match {
        case Success(s) => s
        case Failure(e) => errorAndReturn("invalid return type : " + t, e, VoidTypeSignature)
      }
    }.getOrElse(VoidTypeSignature)

    private def returnBoundSignatures(resolver: NameResolver): List[JTypeSignature] = {
      if (modifiersAST.contains(ExactModifier)) List(returnSignature(resolver))
      else returnBoundSignatures(resolver, returnBoundsAST, Nil)
    }

    private def returnBoundSignatures(resolver: NameResolver, asts: List[TypeName], result: List[JTypeSignature]): List[JTypeSignature] = asts match {
      case bound :: rest => resolver.typeSignature(bound) match {
        case Success(s) => returnBoundSignatures(resolver, rest, s :: result)
        case Failure(e) =>
          error("invalid return bound : " + bound, e)
          returnBoundSignatures(resolver, rest, result)
      }
      case Nil => result.reverse
    }

    private def readClause(name: String, reader: PartialFunction[MethodClause, List[TypeName]], resolver: NameResolver): List[JClassTypeSignature] = clausesAST.collectFirst(reader).map(readClause(name, _, Nil, resolver)).getOrElse(Nil)

    private def readClause(name: String, types: List[TypeName], signatures: List[JClassTypeSignature], resolver: NameResolver): List[JClassTypeSignature] = types match {
      case t :: rest => resolver.classTypeSignature(t) match {
        case Success(sig) => readClause(name, rest, sig :: signatures, resolver)
        case Failure(e) =>
          error("invalid " + name + " clause of method " + this.name, e)
          readClause(name, rest, signatures, resolver)
      }
      case Nil => signatures.reverse
    }
  }

  class IRFormalParameter(ast: FormalParameter, resolver: NameResolver, method: IRProcedure) {
    def name: String = ast.name

    lazy val signature: Try[JParameterSignature] = resolver.parameterSignature(ast, initializerMethod)

    def actualTypeSignature: Option[JTypeSignature] = signature.map(_.actualTypeSignature).toOption

    lazy val initializerMethod: Option[IRParameterInitializer] = for ( snippet <- ast.initializer ) yield {
      IRParameterInitializer(method, this, snippet)
    }
  }

  trait IRMethod extends IRProcedure {
    protected def methodAST: MethodDeclaration

    def name: String = methodAST.name

    protected def modifiersAST: List[Modifier] = methodAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = methodAST.metaParameters

    protected def returnTypeAST: Option[TypeName] = Some(methodAST.returnType)

    protected def returnBoundsAST: List[TypeName] = Nil

    protected def formalParametersAST: List[FormalParameter] = methodAST.formalParameters

    protected def clausesAST: List[MethodClause] = methodAST.clauses

    lazy val methodBody: Option[IRMethodBody] = methodAST.body.map { src =>
      fromTypeSignature(signature.returnType, metaArguments) match {
        case Some(expected) => parseMethodBody(src.snippet, expected, environment) match {
          case Success(v) => v
          case Failure(e) => throw InvalidASTException(s"parse error at the body of method ${declaringClass.name}.$name", e)
        }
        case None => throw InvalidTypeException(s"invalid expected return type for method ${declaringClass.name}.$name")
      }
    }
  }

  case class IRClassMethod(methodAST: MethodDeclaration, declaringClass: IRClass) extends IRMethod with IRClassMember {
    protected def implicitModifiers = 0
  }

  case class IREnumMethod(methodAST: MethodDeclaration, declaringClass: IREnum) extends IRMethod with IREnumMember {
    protected def implicitModifiers = 0
  }

  case class IRInterfaceMethod(methodAST: MethodDeclaration, declaringClass: IRInterface) extends IRMethod with IRInterfaceMember {
    protected def implicitModifiers: Int = accPublic | accAbstract
  }

  trait IRConstructor extends IRProcedure {
    protected def constructorAST: ConstructorDeclaration

    def name: String = CommonNames.constructorName

    protected def modifiersAST: List[Modifier] = constructorAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = constructorAST.metaParameters

    protected def returnTypeAST: Option[TypeName] = None

    protected def returnBoundsAST: List[TypeName] = Nil

    protected def formalParametersAST: List[FormalParameter] = constructorAST.formalParameters

    protected def clausesAST: List[MethodClause] = constructorAST.clauses

    lazy val constructorBody: IRConstructorBody = parseConstructorBody(constructorAST.body.snippet, environment) match {
      case Success(v) => v
      case Failure(e) => throw InvalidASTException(s"parse error at the body of constructor ${declaringClass.name}", e)
    }
  }

  case class IRClassConstructor(constructorAST: ConstructorDeclaration, declaringClass: IRClass) extends IRConstructor with IRClassMember {
    protected def implicitModifiers = 0
  }

  case class IREnumConstructor(constructorAST: ConstructorDeclaration, declaringClass: IREnum) extends IRConstructor with IREnumMember {
    protected def implicitModifiers = 0
  }

  case class IRDSLConstructor(constructorAST: ConstructorDeclaration, declaringClass: IRDSL) extends IRConstructor with IRDSLMember {
    protected def implicitModifiers = 0
  }

  trait IRInstanceInitializer extends IRProcedure {
    protected def instanceInitializerAST: InstanceInitializer

    def name: String = CommonNames.instanceInitializerName

    protected def modifiersAST: List[Modifier] = Nil

    protected def metaParametersAST: List[MetaParameter] = Nil

    protected def returnTypeAST: Option[TypeName] = None

    protected def returnBoundsAST: List[TypeName] = Nil

    protected def formalParametersAST: List[FormalParameter] = Nil

    protected def clausesAST: List[MethodClause] = Nil

    protected def implicitModifiers: Int = 0

    lazy val initializerBody: IRInitializerBody = {
      parseInitializerBody(instanceInitializerAST.block.snippet, environment) match {
        case Success(e) => e
        case Failure(e) => throw InvalidASTException(s"parse error at instance initializer of ${declaringClass.name}", e)
      }
    }
  }

  case class IRClassInstanceInitializer(instanceInitializerAST: InstanceInitializer, declaringClass: IRClass) extends IRInstanceInitializer with IRClassMember

  case class IREnumInstanceInitializer(instanceInitializerAST: InstanceInitializer, declaringClass: IREnum) extends IRInstanceInitializer with IREnumMember

  trait IRStaticInitializer extends IRProcedure {
    protected def staticInitializerAST: StaticInitializer

    def name: String = CommonNames.classInitializerName

    protected def modifiersAST: List[Modifier] = Nil

    protected def metaParametersAST: List[MetaParameter] = Nil

    protected def returnTypeAST: Option[TypeName] = None

    protected def returnBoundsAST: List[TypeName] = Nil

    protected def formalParametersAST: List[FormalParameter] = Nil

    protected def clausesAST: List[MethodClause] = Nil

    protected def implicitModifiers: Int = accStatic

    lazy val initializerBody: IRInitializerBody = {
      parseInitializerBody(staticInitializerAST.block.snippet, environment) match {
        case Success(e) => e
        case Failure(e) => throw InvalidASTException(s"parse error at static initializer of ${declaringClass.name}", e)
      }
    }
  }

  case class IRClassStaticInitializer(staticInitializerAST: StaticInitializer, declaringClass: IRClass) extends IRStaticInitializer with IRClassMember

  case class IREnumStaticInitializer(staticInitializerAST: StaticInitializer, declaringClass: IREnum) extends IRStaticInitializer with IREnumMember

  case class IROperator(protected val operatorAST: OperatorDeclaration, declaringClass: IRDSL) extends IRProcedure with IRDSLMember {
    def dsl: IRDSL = declaringClass

    lazy val name: String = operatorAST.label.getOrElse("ProteanOperator$" + generateUniqueId)

    protected def modifiersAST: List[Modifier] = operatorAST.modifiers

    protected def metaParametersAST: List[MetaParameter] = operatorAST.metaParameters

    protected def returnTypeAST: Option[TypeName] = Some(operatorAST.returnType)

    protected def returnBoundsAST: List[TypeName] = operatorAST.returnBounds

    protected def formalParametersAST: List[FormalParameter] = operatorAST.formalParameters

    protected def clausesAST: List[MethodClause] = operatorAST.clauses

    protected def implicitModifiers: Int = 0

    override def syntax = Some(operatorSyntax)

    lazy val operatorSyntax: JSyntaxDef = {
      if (modifiersAST.contains(LiteralModifier)) JLiteralSyntaxDef(priority, pattern)
      else JExpressionSyntaxDef(priority, pattern)
    }

    lazy val priority: JPriority = operatorAST.priority.flatMap(resolver.priority).getOrElse {
      JPriority(SimpleClassTypeSignature(dsl.internalName, Nil), "ProteanOperatorPriority$" + generateUniqueId)
    }

    lazy val pattern: List[JSyntaxElementDef] = operatorAST.syntax map {
      case OperatorName(n) => JOperatorNameDef(n)
      case RegexName(n) => JRegexNameDef(n)
      case MetaValueRef(n, p) => JMetaValueRefDef(n, p.flatMap(resolver.priority))
      case Operand(p) => JOperandDef(p.flatMap(resolver.priority))
      case Repetition0(p) => JRepetition0Def(p.flatMap(resolver.priority))
      case Repetition1(p) => JRepetition1Def(p.flatMap(resolver.priority))
      case OptionalOperand(p) => JOptionalOperandDef(p.flatMap(resolver.priority))
      case AndPredicate(t, p) => JAndPredicateDef(predicateSignature(t), p.flatMap(resolver.priority))
      case NotPredicate(t, p) => JNotPredicateDef(predicateSignature(t), p.flatMap(resolver.priority))
    }

    lazy val operatorBody: Option[IRMethodBody] = operatorAST.body.map { src =>
      fromTypeSignature(signature.returnType, metaArguments) match {
        case Some(expected) => parseMethodBody(src.snippet, expected, environment) match {
          case Success(v) => v
          case Failure(e) => throw InvalidASTException(s"parse error at the body of operator ${declaringClass.name}.$name", e)
        }
        case None => throw InvalidTypeException(s"invalid expected return type for operator ${declaringClass.name}.$name")
      }
    }

    private def predicateSignature(t: TypeName): JTypeSignature = {
      successOrError(resolver.typeSignature(t), "invalid predicate type : " + t, VoidTypeSignature)
    }
  }

  case class IRPriorities(prioritiesAST: PrioritiesDeclaration, declaringDSL: IRDSL) {
    def priorityNames: List[String] = prioritiesAST.names

    lazy val constraints: List[List[JPriority]] = prioritiesAST.constraints.map(declaringDSL.staticResolver.constraint)
  }


  sealed trait IRSyntheticMethod extends JMethodDef with IRMember {
    def mod: JModifier = JModifier(modifiers)

    def syntax: Option[JSyntaxDef] = None

    def signature: JMethodSignature = JMethodSignature(metaParameters, parameterSignatures, returnTypeSignature, Nil, Nil, Nil, Nil, Nil)

    protected def modifiers: Int

    protected def metaParameters: List[FormalMetaParameter]

    protected def parameterSignatures: List[JParameterSignature]

    protected def returnTypeSignature: JTypeSignature
  }

  case class IRFieldInitializer (field: IRField, snippet: ExpressionSnippet) extends IRSyntheticMethod {
    protected def modifiers: Int = {
      if (field.isStatic) accPublic | accStatic | accFinal
      else accPublic | accFinal
    }

    protected def metaParameters: List[FormalMetaParameter] = Nil

    protected def returnTypeSignature: JTypeSignature = field.signature

    protected def parameterSignatures: List[JParameterSignature] = Nil

    lazy val name: String = field.name + "$init$" + generateUniqueId

    def declaringClass: IRModule = field.declaringClass

    lazy val expression: IRExpression = {
      if (field.isStatic) fromTypeSignature(field.signature, Map.empty) match {
        case Some(expected) => parseExpression(snippet.snippet, expected, declaringClass.staticEnvironment) match {
          case Success(v) => v
          case Failure(e) => throw InvalidASTException(s"parse error at the initializer of field ${declaringClass.name}.$name", e)
        }
        case None           => throw InvalidTypeException(s"invalid expected type for the initializer of field ${field.declaringClass.name}.$name")
      }
      else fromTypeSignature(field.signature, declaringClass.metaParameters) match {
        case Some(expected) => parseExpression(snippet.snippet, expected, declaringClass.instanceEnvironment) match {
          case Success(v) => v
          case Failure(e) => throw InvalidASTException(s"parse error at the initializer of field ${declaringClass.name}.$name", e)
        }
        case None           => throw InvalidTypeException(s"invalid expected type for the initializer of field ${field.declaringClass.name}.$name")
      }
    }
  }

  case class IRParameterInitializer (method: IRProcedure, param: IRFormalParameter, snippet: ExpressionSnippet) extends IRSyntheticMethod {
    protected def modifiers: Int = {
      if (method.isStatic) accPublic | accStatic | accFinal
      else accPublic | accFinal
    }

    protected def metaParameters: List[FormalMetaParameter] = method.signature.metaParams

    protected def parameterSignatures: List[JParameterSignature] = Nil

    protected def returnTypeSignature: JTypeSignature = param.actualTypeSignature.getOrElse {
      throw InvalidTypeException(s"invalid type signature for the parameter initializer of method ${declaringClass.name}.${method.name}")
    }

    lazy val name: String = method.name + "$init$" + generateUniqueId

    def declaringClass: IRModule = method.declaringClass

    lazy val expression: IRExpression = {
      fromTypeSignature(returnTypeSignature, method.metaArguments) match {
        case Some(expected) => parseExpression(snippet.snippet, expected, environment) match {
          case Success(v) => v
          case Failure(e) => throw InvalidASTException(s"parse error at the parameter initializer of method ${declaringClass.name}.$name", e)
        }
        case None           => throw InvalidTypeException(s"invalid expected type for the parameter initializer of method ${declaringClass.name}.${method.name}")
      }
    }

    private def environment: ModuleEnvironment = {
      if (isStatic) declaringClass.staticEnvironment
      else declaringClass.instanceEnvironment
    }
  }

  object IRModifiers {
    def mod(modifiers: List[Modifier]): JModifier = JModifier(modifiers.foldRight(0)((m, flags) => flags | flag(m)))

    private def flag(modifier: Modifier): Int = modifier match {
      case PublicModifier => accPublic
      case PrivateModifier => accPrivate
      case ProtectedModifier => accProtected
      case StaticModifier => accStatic
      case FinalModifier => accFinal
      case SynchronizedModifier => accSynchronized
      case VolatileModifier => accVolatile
      case TransientModifier => accTransient
      case NativeModifier => accNative
      case AbstractModifier => accAbstract
      case StrictFPModifier => accStrict
      case _ => 0
    }
  }

  case class IRDSLInfo(priorities: List[String], constraints: List[List[JPriority]], withDSLs: List[JClass])

}