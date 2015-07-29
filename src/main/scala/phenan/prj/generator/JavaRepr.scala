package phenan.prj.generator

import phenan.prj._
import phenan.prj.exception.InvalidASTException
import phenan.prj.ir._

import phenan.util._

import CommonNames._

object JavaRepr {

  trait JavaFile {
    def packageName: Option[String]
    def modules: List[ModuleDef]
  }

  type ModuleDef = ClassDef :|: EnumDef :|: InterfaceDef :|: UNil

  type ClassMember = FieldDef :|: MethodDef :|: ConstructorDef :|: InstanceInitializerDef :|: StaticInitializerDef :|: ModuleDef :|: UNil

  trait ClassDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def typeParameters: List[TypeParam]
    def superType: ClassSig
    def interfaces: List[ClassSig]
    def members: List[ClassMember]
  }

  trait EnumDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def interfaces: List[ClassSig]
    def constants: List[EnumConstantDef]
    def members: List[ClassMember]
  }

  trait InterfaceDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def typeParameters: List[TypeParam]
    def superInterfaces: List[ClassSig]
    def members: List[ClassMember]
  }

  trait FieldDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def fieldType: TypeSig
    def name: String
    def initializer: Option[Expression]
  }

  trait MethodDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def typeParameters: List[TypeParam]
    def returnType: TypeSig
    def name: String
    def parameters: List[Param]
    def throws: List[TypeSig]
    def body: Option[Block]
  }

  trait ConstructorDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def typeParameters: List[TypeParam]
    def className: String
    def parameters: List[Param]
    def throws: List[TypeSig]
    def body: Block
  }

  trait InstanceInitializerDef {
    def body: Block
  }

  trait StaticInitializerDef {
    def body: Block
  }

  trait EnumConstantDef {
    def name: String
  }

  trait Param {
    def parameterType: TypeSig
    def name: String
  }

  type Statement = Block :|: LocalDeclarationStatement :|: IfStatement :|: WhileStatement :|: ForStatement :|: ReturnStatement :|: ExpressionStatement :|: UNil

  trait LocalDeclarationStatement {
    def declaration: LocalDeclaration
  }
  
  trait LocalDeclaration {
    def localType: TypeSig
    def declarators: List[LocalDeclarator]
  }

  trait LocalDeclarator {
    def name: String
    def dim: Int
    def initializer: Option[Expression]
  }

  trait IfStatement {
    def condition: Expression
    def thenStatement: Statement
    def elseStatement: Option[Statement]
  }

  trait WhileStatement {
    def condition: Expression
    def loopBody: Statement
  }

  type ForStatement = NormalForStatement :|: EnhancedForStatement :|: UNil

  type ForInit = LocalDeclaration :|: List[Expression] :|: UNil

  trait NormalForStatement {
    def forInit: ForInit
    def condition: Option[Expression]
    def update: List[Expression]
    def loopBody: Statement
  }

  trait EnhancedForStatement {
    def elementType: TypeSig
    def name: String
    def dim: Int
    def iterable: Expression
    def loopBody: Statement
  }

  trait ReturnStatement {
    def returnValue: Expression
  }

  trait Block {
    def statements: List[Statement]
  }

  trait ExpressionStatement {
    def statementExpression: Expression
  }

  type Expression = MethodCall :|: FieldAccess :|: CastExpression :|: ArrayAccess :|: NewExpression :|: NewArray :|: ArrayInit :|: LocalRef :|: ThisRef.type :|: JavaLiteral :|: UNil

  type Receiver = Expression :|: ClassRef :|: SuperRef.type :|: UNil

  type Assignment = SimpleAssignment

  trait SimpleAssignment {
    def left: Expression
    def right: Expression
  }

  trait MethodCall {
    def receiver: Receiver
    def typeArguments: List[TypeArg]
    def methodName: String
    def arguments: List[Expression]
  }

  trait FieldAccess {
    def receiver: Receiver
    def fieldName: String
  }

  trait CastExpression {
    def destType: TypeSig
    def castedExpression: Expression
  }

  trait ArrayAccess {
    def array: Expression
    def index: Expression
  }

  trait NewExpression {
    def typeArguments: List[TypeArg]
    def constructType: TypeSig
    def arguments: List[Expression]
  }

  trait NewArray {
    def componentType: TypeSig
    def arraySize: List[Expression]
    def dim: Int
  }

  trait ArrayInit {
    def componentType: TypeSig
    def dim: Int
    def components: List[Expression]
  }

  trait LocalRef {
    def name: String
  }

  trait ClassRef {
    def name: String
  }

  object SuperRef
  object ThisRef

  type JavaLiteral = ClassLiteral :|: Literal[String] :|: Literal[Char] :|: Literal[Int] :|: Literal[Long] :|: Literal[Boolean] :|: UNil

  trait ClassLiteral {
    def className: String
    def dim: Int
  }

  trait Literal[T] {
    def value: T
  }

  trait JavaAnnotation {
    def name: String
    def arguments: Map[String, AnnotationElement]
  }

  type AnnotationElement = ElementArray :|: JavaAnnotation :|: JavaLiteral :|: EnumConstRef :|: UNil

  trait ElementArray {
    def elements: List[AnnotationElement]
  }

  trait EnumConstRef {
    def enumName: String
    def constantName: String
  }

  trait TypeParam {
    def name: String
    def bounds: List[TypeSig]
  }

  type TypeArg = TypeSig :|: Wildcard :|: UNil

  type Wildcard = UnboundWildcard.type :|: UpperBoundWildcard :|: LowerBoundWildcard :|: UNil

  type TypeSig = ClassSig :|: ArraySig :|: TypeVariableSig :|: PrimitiveSig :|: UNil

  type ClassSig = TopLevelClassSig :|: MemberClassSig :|: UNil

  trait TopLevelClassSig {
    def className: String
    def typeArguments: List[TypeArg]
  }

  trait MemberClassSig {
    def outer: ClassSig
    def className: String
    def typeArguments: List[TypeArg]
  }

  trait ArraySig {
    def component: TypeSig
  }

  trait TypeVariableSig {
    def name: String
  }

  trait PrimitiveSig {
    def name: String
  }

  object UnboundWildcard

  trait UpperBoundWildcard {
    def bound: TypeSig
  }

  trait LowerBoundWildcard {
    def bound: TypeSig
  }

  /* AST transformation : ProteaJ IR ==> Java AST */

  def javaFile (file: IRFile): JavaFile = new JavaFile {
    def packageName = file.packageName.map(_.names.mkString("."))
    def modules = file.topLevelModules.map(moduleDef)
  }

  def moduleDef (clazz: IRModule): ModuleDef = clazz match {
    case cls: IRTopLevelClass     => Union[ModuleDef](classDef(cls))
    case enm: IRTopLevelEnum      => Union[ModuleDef](enumDef(enm))
    case ifc: IRTopLevelInterface => Union[ModuleDef](interfaceDef(ifc))
    case dsl: IRTopLevelDSL       => Union[ModuleDef](dslDef(dsl))
  }

  def classDef (clazz: IRClass): ClassDef = new ClassDef {
    def annotations = Annotations.classAnnotations(clazz)
    def modifiers = clazz.mod
    def name = clazz.simpleName
    def typeParameters = typeParams(clazz.signature.metaParams)
    def superType = classSig(clazz.signature.superClass)
    def interfaces = clazz.signature.interfaces.map(classSig)
    def members = clazz.declaredMembers.map(classMember) ++ clazz.syntheticMethods.map(syntheticMember)
  }

  def enumDef (enum: IREnum): EnumDef = new EnumDef {
    def annotations = Annotations.enumAnnotations(enum)
    def modifiers = enum.mod
    def name = enum.simpleName
    def interfaces = enum.signature.interfaces.map(classSig)
    def constants = enum.enumConstants.map(enumConstantDef)
    def members = enum.enumMembers.map(enumMember)
  }

  def interfaceDef (interface: IRInterface): InterfaceDef = new InterfaceDef {
    def annotations = Annotations.interfaceAnnotations(interface)
    def modifiers = interface.mod
    def name = interface.simpleName
    def typeParameters = typeParams(interface.signature.metaParams)
    def superInterfaces = interface.signature.interfaces.map(classSig)
    def members = interface.declaredMembers.map(interfaceMember)
  }

  def dslDef (dsl: IRDSL): ClassDef = new ClassDef {
    def annotations = Annotations.dslAnnotations(dsl)
    def modifiers = dsl.mod
    def name = dsl.simpleName
    def typeParameters = Nil
    def superType = objectClassSig
    def interfaces = Nil
    def members = dsl.declaredMembers.flatMap(dslMember) ++ dsl.syntheticMethods.map(syntheticMember)
  }

  def contextDef (context: IRContext): ClassDef = new ClassDef {
    def annotations = Annotations.contextAnnotations(context)
    def modifiers = context.mod
    def name = context.simpleName
    def typeParameters = typeParams(context.signature.metaParams)
    def superType = objectClassSig
    def interfaces = Nil
    def members = context.declaredMembers.map(contextMember) ++ context.syntheticMethods.map(syntheticMember)
  }

  def classMember (member: IRClassMember): ClassMember = member match {
    case field: IRClassField             => Union[ClassMember](fieldDef(field))
    case method: IRClassMethod           => Union[ClassMember](methodDef(method))
    case constructor: IRClassConstructor => Union[ClassMember](constructorDef(constructor))
    case iin: IRClassInstanceInitializer => Union[ClassMember](instanceInitializerDef(iin))
    case sin: IRClassStaticInitializer   => Union[ClassMember](staticInitializerDef(sin))
    case module: IRModule                => Union[ClassMember](moduleDef(module))
  }

  def enumMember (member: IREnumMember): ClassMember = member match {
    case field: IREnumField             => Union[ClassMember](fieldDef(field))
    case method: IREnumMethod           => Union[ClassMember](methodDef(method))
    case constructor: IREnumConstructor => Union[ClassMember](constructorDef(constructor))
    case iin: IREnumInstanceInitializer => Union[ClassMember](instanceInitializerDef(iin))
    case sin: IREnumStaticInitializer   => Union[ClassMember](staticInitializerDef(sin))
    case module: IRModule               => Union[ClassMember](moduleDef(module))
    case _ => throw InvalidASTException("invalid enum declaration AST")
  }

  def interfaceMember (member: IRInterfaceMember): ClassMember = member match {
    case field: IRInterfaceField   => Union[ClassMember](fieldDef(field))
    case method: IRInterfaceMethod => Union[ClassMember](methodDef(method))
    case module: IRModule          => Union[ClassMember](moduleDef(module))
  }

  def dslMember (member: IRDSLMember): Option[ClassMember] = member match {
    case field: IRDSLField       => Some(Union[ClassMember](fieldDef(field)))
    case operator: IRDSLOperator => Some(Union[ClassMember](operatorDef(operator)))
    case context: IRContext      => Some(Union[ClassMember](Union[ModuleDef](contextDef(context))))
    case _: IRDSLPriorities      => None
  }

  def contextMember (member: IRContextMember): ClassMember = member match {
    case field: IRContextField             => Union[ClassMember](fieldDef(field))
    case operator: IRContextOperator       => Union[ClassMember](operatorDef(operator))
    case constructor: IRContextConstructor => Union[ClassMember](constructorDef(constructor))
  }

  def syntheticMember (synthetic: IRSyntheticMethod): ClassMember = synthetic match {
    case ini: IRParameterInitializer => Union[ClassMember](parameterInitializerDef(ini))
  }

  def fieldDef (field: IRField): FieldDef = new FieldDef {
    def annotations = Annotations.fieldAnnotations(field)
    def modifiers = field.mod
    def fieldType = typeSig(field.signature)
    def name = field.name
    def initializer: Option[Expression] = field.initializer.map(expression)
  }

  def methodDef (method: IRMethod): MethodDef = new MethodDef {
    def annotations = Annotations.methodAnnotations(method)
    def modifiers = method.mod
    def typeParameters = typeParams(method.signature.metaParams)
    def returnType = typeSig(method.signature.returnType)
    def name = method.name
    def parameters = method.parameters.map(parameter)
    def throws = method.signature.throwTypes.map(typeSig)
    def body = method.methodBody.map(methodBody)
  }

  def operatorDef (operator: IROperator): MethodDef = new MethodDef {
    def annotations = Annotations.operatorAnnotations(operator)
    def modifiers = operator.mod
    def typeParameters = typeParams(operator.signature.metaParams)
    def returnType = typeSig(operator.signature.returnType)
    def name = operator.name
    def parameters = operator.parameters.map(parameter)
    def throws = operator.signature.throwTypes.map(typeSig)
    def body = operator.operatorBody.map(methodBody)
  }
  
  def parameterInitializerDef (initializer: IRParameterInitializer): MethodDef = new MethodDef {
    def annotations = Annotations.paramInitializerAnnotations(initializer)
    def modifiers = initializer.mod
    def typeParameters = Nil
    def returnType = typeSig(initializer.signature.returnType)
    def name = initializer.name
    def parameters = Nil
    def throws = Nil
    def body = initializer.expression.map(parameterInitializer)
  }

  def constructorDef (constructor: IRConstructor): ConstructorDef = new ConstructorDef {
    def annotations = Annotations.constructorAnnotations(constructor)
    def modifiers = constructor.mod
    def typeParameters = typeParams(constructor.signature.metaParams)
    def className = constructor.declaringClass.simpleName
    def parameters = constructor.parameters.map(parameter)
    def throws = constructor.signature.throwTypes.map(typeSig)
    def body = constructor.constructorBody.map(constructorBody).getOrElse {
      throw InvalidASTException("constructor must have its body")
    }
  }

  def instanceInitializerDef (iin: IRInstanceInitializer): InstanceInitializerDef = new InstanceInitializerDef {
    def body = iin.initializerBody.map(initializerBody).getOrElse {
      throw InvalidASTException("invalid instance initializer")
    }
  }

  def staticInitializerDef (sin: IRStaticInitializer): StaticInitializerDef = new StaticInitializerDef {
    def body = sin.initializerBody.map(initializerBody).getOrElse {
      throw InvalidASTException("invalid static initializer")
    }
  }

  def enumConstantDef (constant: IREnumConstant): EnumConstantDef = new EnumConstantDef {
    def name = constant.name
  }

  def parameter (param: IRFormalParameter): Param = new Param {
    def parameterType = param.actualTypeSignature.map(typeSig).getOrElse {
      throw InvalidASTException("invalid parameter type")
    }
    def name = param.name
  }

  def methodBody (body: IRMethodBody): Block = block(body.block)

  def constructorBody (body: IRConstructorBody): Block = ???

  def initializerBody (body: IRInitializerBody): Block = block(body.block)

  def parameterInitializer (body: IRExpression): Block = {
    block(List(Union[Statement](returnStatement(expression(body)))))
  }

  /* statements */

  def block (b: IRBlock): Block = block(b.statements.map(statement))

  def block (sts: List[Statement]): Block = new Block {
    def statements = sts
  }

  def statement (stmt: IRStatement): Statement = stmt match {
    case b: IRBlock                     => Union[Statement](block(b))
    case l: IRLocalDeclarationStatement => Union[Statement](localDeclarationStatement(l))
    case i: IRIfStatement               => Union[Statement](ifStatement(i))
    case w: IRWhileStatement            => Union[Statement](whileStatement(w))
    case f: IRForStatement              => Union[Statement](forStatement(f))
    case a: IRActivateStatement         => ???
    case r: IRReturnStatement           => Union[Statement](returnStatement(r))
    case e: IRExpressionStatement       => Union[Statement](expressionStatement(e))
  }

  def localDeclarationStatement (stmt: IRLocalDeclarationStatement): LocalDeclarationStatement = new LocalDeclarationStatement {
    def declaration = localDeclaration(stmt.declaration)
  }

  def localDeclaration (declaration: IRLocalDeclaration): LocalDeclaration = new LocalDeclaration {
    def localType = typeToSig(declaration.localType)
    def declarators = declaration.declarators.map(localDeclarator)
  }

  def localDeclarator (declarator: IRVariableDeclarator): LocalDeclarator = new LocalDeclarator {
    def name = declarator.name
    def dim = declarator.dim
    def initializer = declarator.init.map(expression)
  }

  def ifStatement (stmt: IRIfStatement): IfStatement = new IfStatement {
    def condition = expression(stmt.condition)
    def thenStatement = statement(stmt.thenStatement)
    def elseStatement = stmt.elseStatement.map(statement)
  }

  def whileStatement (stmt: IRWhileStatement): WhileStatement = new WhileStatement {
    def condition = expression(stmt.condition)
    def loopBody = statement(stmt.statement)
  }

  def forStatement (stmt: IRForStatement): ForStatement = stmt match {
    case s: IRNormalForStatement   => Union[ForStatement](normalForStatement(s))
    case s: IRAncientForStatement  => Union[ForStatement](ancientForStatement(s))
    case s: IREnhancedForStatement => Union[ForStatement](enhancedForStatement(s))
  }

  def normalForStatement (stmt: IRNormalForStatement): NormalForStatement = new NormalForStatement {
    def forInit = Union[ForInit](localDeclaration(stmt.local))
    def condition = stmt.condition.map(expression)
    def update = stmt.update.map(expression)
    def loopBody = statement(stmt.statement)
  }

  def ancientForStatement (stmt: IRAncientForStatement): NormalForStatement = new NormalForStatement {
    def forInit = Union[ForInit](stmt.init.map(expression))
    def condition = stmt.condition.map(expression)
    def update = stmt.update.map(expression)
    def loopBody = statement(stmt.statement)
  }

  def enhancedForStatement (stmt: IREnhancedForStatement): EnhancedForStatement = new EnhancedForStatement {
    def elementType = typeToSig(stmt.elementType)
    def name = stmt.name
    def dim = stmt.dim
    def iterable = expression(stmt.iterable)
    def loopBody = statement(stmt.statement)
  }

  def returnStatement (stmt: IRReturnStatement): ReturnStatement = new ReturnStatement {
    def returnValue = expression(stmt.expression)
  }

  def returnStatement (expression: Expression): ReturnStatement = new ReturnStatement {
    def returnValue = expression
  }

  def expressionStatement (stmt: IRExpressionStatement): ExpressionStatement = new ExpressionStatement {
    def statementExpression = expression(stmt.expression)
  }

  /* expressions */

  def expression (expression: IRExpression): Expression = ???

  /* literals */

  def javaLiteral (literal: IRJavaLiteral): JavaLiteral = literal match {
    case c: IRClassLiteral   => Union[JavaLiteral](classLiteral(c))
    case s: IRStringLiteral  => Union[JavaLiteral](stringLiteral(s))
    case c: IRCharLiteral    => Union[JavaLiteral](charLiteral(c))
    case i: IRIntLiteral     => Union[JavaLiteral](intLiteral(i))
    case j: IRLongLiteral    => Union[JavaLiteral](longLiteral(j))
    case z: IRBooleanLiteral => Union[JavaLiteral](booleanLiteral(z))
  }

  def classLiteral (c: IRClassLiteral): ClassLiteral = c match {
    case IRObjectClassLiteral(clazz, d) => classLiteral(clazz.name, d)
    case IRPrimitiveClassLiteral(primitive, d) => classLiteral(primitive.name, d)
  }

  def classLiteral (clazz: JClass): ClassLiteral = classLiteral(clazz.name, 0)

  private def classLiteral (name: String, dimension: Int): ClassLiteral = new ClassLiteral {
    def className: String = name
    def dim: Int = dimension
  }

  def stringLiteral (lit: IRStringLiteral): Literal[String] = literal(lit.value)

  def stringLiteral (v: String): Literal[String] = literal(v)

  def charLiteral (lit: IRCharLiteral): Literal[Char] = literal(lit.value)

  def intLiteral (lit: IRIntLiteral): Literal[Int] = literal(lit.value)

  def longLiteral (lit: IRLongLiteral): Literal[Long] = literal(lit.value)

  def booleanLiteral (lit: IRBooleanLiteral): Literal[Boolean] = literal(lit.value)

  private def literal [T] (t: T): Literal[T] = new Literal[T] {
    def value = t
  }

  /* signatures */

  def typeToSig (t: JType): TypeSig = t match {
    case obj: JObjectType    => Union[TypeSig](objectType(obj))
    case ary: JArrayType     => Union[TypeSig](arrayType(ary))
    case prm: JPrimitiveType => Union[TypeSig](primitiveType(prm))
    case tvr: JTypeVariable  => Union[TypeSig](typeVariable(tvr))
    case cap: JCapturedWildcardType => throw InvalidASTException("captured wildcard is found in generated Java AST")
    case unb: JUnboundTypeVariable  => throw InvalidASTException("unbound type variable is found in generated Java AST")
  }

  def objectType (obj: JObjectType): ClassSig = Union[ClassSig](topLevelClassObjectType(obj))

  def topLevelClassObjectType (obj: JObjectType): TopLevelClassSig = new TopLevelClassSig {
    def className: String = obj.erase.name
    def typeArguments: List[TypeArg] = obj.erase.signature.metaParams.flatMap { param =>
      metaArgument(obj.env.getOrElse(param.name, throw InvalidASTException("invalid type argument")))
    }
  }

  def arrayType (ary: JArrayType): ArraySig = new ArraySig {
    def component: TypeSig = typeToSig(ary.componentType)
  }

  def primitiveType (prm: JPrimitiveType): PrimitiveSig = primitiveSig(prm.name)

  def typeVariable (tvr: JTypeVariable): TypeVariableSig = new TypeVariableSig {
    def name: String = tvr.name
  }

  def metaArgument (arg: MetaArgument): Option[TypeArg] = arg match {
    case ref: JRefType   => Some(Union[TypeArg](typeToSig(ref)))
    case wild: JWildcard => Some(Union[TypeArg](wildcard(wild)))
    case meta: MetaValue => None
  }

  def wildcard (wild: JWildcard): Wildcard = wild match {
    case JWildcard(Some(ub), _) => Union[Wildcard](new UpperBoundWildcard { def bound: TypeSig = typeToSig(ub) })
    case JWildcard(_, Some(lb)) => Union[Wildcard](new LowerBoundWildcard { def bound: TypeSig = typeToSig(lb) })
    case JWildcard(None, None)  => Union[Wildcard](UnboundWildcard)
  }

  def typeParams (mps: List[FormalMetaParameter]): List[TypeParam] = mps.filter(_.metaType == JTypeSignature.typeTypeSig).map(typeParam)

  def typeParam (mp: FormalMetaParameter): TypeParam = new TypeParam {
    def name = mp.name
    def bounds = mp.bounds.map(typeSig)
  }

  def typeSig (signature: JTypeSignature): TypeSig = signature match {
    case c: JClassTypeSignature        => Union[TypeSig](classSig(c))
    case a: JArrayTypeSignature        => Union[TypeSig](arraySig(a))
    case p: JPrimitiveTypeSignature    => Union[TypeSig](primitiveSig(p))
    case v: JTypeVariableSignature     => Union[TypeSig](typeVariableSig(v))
    case c: JCapturedWildcardSignature => throw InvalidASTException("captured wildcard is found in generated Java code")
  }

  def classSig (signature: JClassTypeSignature): ClassSig = signature match {
    case s: SimpleClassTypeSignature => Union[ClassSig](topLevelClassSig(s))
    case m: MemberClassTypeSignature => Union[ClassSig](memberClassSig(m))
  }

  def objectClassSig: ClassSig = classSig(JTypeSignature.objectTypeSig)

  def topLevelClassSig (signature: SimpleClassTypeSignature): TopLevelClassSig = new TopLevelClassSig {
    def className: String = signature.internalName.replace('/', '.')
    def typeArguments: List[TypeArg] = signature.args.flatMap(typeArg)
  }

  def memberClassSig (signature: MemberClassTypeSignature): MemberClassSig = new MemberClassSig {
    def outer: ClassSig = classSig(signature.outer)
    def className: String = signature.clazz
    def typeArguments: List[TypeArg] = signature.args.flatMap(typeArg)
  }

  def arraySig (signature: JArrayTypeSignature): ArraySig = new ArraySig {
    def component: TypeSig = typeSig(signature.component)
  }

  def primitiveSig (signature: JPrimitiveTypeSignature): PrimitiveSig = signature match {
    case ByteTypeSignature   => primitiveSig("byte")
    case CharTypeSignature   => primitiveSig("char")
    case DoubleTypeSignature => primitiveSig("double")
    case FloatTypeSignature  => primitiveSig("float")
    case IntTypeSignature    => primitiveSig("int")
    case LongTypeSignature   => primitiveSig("long")
    case ShortTypeSignature  => primitiveSig("short")
    case BoolTypeSignature   => primitiveSig("boolean")
    case VoidTypeSignature   => primitiveSig("void")
  }

  private def primitiveSig (primitiveName: String): PrimitiveSig = new PrimitiveSig {
    def name = primitiveName
  }

  def typeVariableSig (v: JTypeVariableSignature) = new TypeVariableSig {
    def name = v.name
  }

  def typeArg (arg: JTypeArgument): Option[TypeArg] = arg match {
    case signature: JTypeSignature      => Some(Union[TypeArg](typeSig(signature)))
    case wild: WildcardArgument         => Some(Union[TypeArg](wildcard(wild)))
    case metaVar: MetaVariableSignature => None
  }

  def wildcard (wild: WildcardArgument): Wildcard = wild match {
    case WildcardArgument(Some(ub), _) => Union[Wildcard](new UpperBoundWildcard { def bound = typeSig(ub) })
    case WildcardArgument(_, Some(lb)) => Union[Wildcard](new LowerBoundWildcard { def bound = typeSig(lb) })
    case WildcardArgument(None, None)  => Union[Wildcard](UnboundWildcard)
  }

  /* annotation */

  def annotation (ann: IRAnnotation): JavaAnnotation = new JavaAnnotation {
    def name: String = ann.annotationClass.name
    def arguments: Map[String, AnnotationElement] = ann.args.mapValues(annotationElement)
  }

  def annotation (n: String, args: Map[String, AnnotationElement]): JavaAnnotation = new JavaAnnotation {
    def name: String = n
    def arguments: Map[String, AnnotationElement] = args
  }

  def annotationElement (e: IRAnnotationElement): AnnotationElement = e match {
    case array: IRAnnotationElementArray => Union[AnnotationElement](elementArray(array))
    case ann: IRAnnotation        => Union[AnnotationElement](annotation(ann))
    case literal: IRJavaLiteral   => Union[AnnotationElement](javaLiteral(literal))
    case const: IREnumConstantRef => Union[AnnotationElement](enumConstRef(const))
  }

  def elementArray (array: IRAnnotationElementArray): ElementArray = new ElementArray {
    def elements = array.array.map(annotationElement)
  }

  def elementArray (es: List[AnnotationElement]): ElementArray = new ElementArray {
    def elements = es
  }

  def enumConstRef (const: IREnumConstantRef): EnumConstRef = new EnumConstRef {
    def enumName = const.field.declaringClass.name
    def constantName = const.field.name
  }

  def enumConstRef (enum: String, const: String): EnumConstRef = new EnumConstRef {
    def enumName = enum
    def constantName = const
  }

  object Annotations {
    def classAnnotations (clazz: IRClass): List[JavaAnnotation] = {
      if (clazz.isDSL) classLikeAnnotations(clazz) :+ dslAnnotation(clazz.declaredPriorities, clazz.priorityConstraints, clazz.withDSLs)
      else classLikeAnnotations(clazz)
    }

    def enumAnnotations (enum: IREnum): List[JavaAnnotation] = classLikeAnnotations(enum)

    def interfaceAnnotations (interface: IRInterface): List[JavaAnnotation] = classLikeAnnotations(interface)

    def dslAnnotations (dsl: IRDSL): List[JavaAnnotation] = {
      classLikeAnnotations(dsl) :+ dslAnnotation(dsl.declaredPriorities, dsl.priorityConstraints, dsl.withDSLs)
    }

    def contextAnnotations (context: IRContext): List[JavaAnnotation] = {
      classLikeAnnotations(context) :+ contextAnnotation
    }

    def fieldAnnotations (field: IRField): List[JavaAnnotation] = {
      except(fieldSigClassName)(field.annotations) :+ fieldSignatureAnnotation(field.signature)
    }

    def methodAnnotations (method: IRMethod): List[JavaAnnotation] = method.syntax match {
      case Some(syntax) => methodLikeAnnotations(method) :+ operatorAnnotation(syntax)
      case None         => methodLikeAnnotations(method)
    }

    def operatorAnnotations (operator: IROperator): List[JavaAnnotation] = {
      methodLikeAnnotations(operator) :+ operatorAnnotation(operator.operatorSyntax)
    }

    def constructorAnnotations (constructor: IRConstructor): List[JavaAnnotation] = methodLikeAnnotations(constructor)

    def paramInitializerAnnotations (initializer: IRParameterInitializer): List[JavaAnnotation] = List(methodSignatureAnnotation(initializer.signature))

    private def classLikeAnnotations (clazz: IRModule): List[JavaAnnotation] = {
      except(classSigClassName, dslClassName, contextClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature)
    }

    private def methodLikeAnnotations (method: IRProcedure): List[JavaAnnotation] = {
      except(methodSigClassName, operatorClassName)(method.annotations) :+ methodSignatureAnnotation(method.signature)
    }

    private def except (names: String*)(as: List[IRAnnotation]): List[JavaAnnotation] = as.filterNot { ann => names.contains(ann.annotationClass.internalName) }.map(annotation)

    private def classSignatureAnnotation (sig: JClassSignature): JavaAnnotation = {
      mkAnnotation(classSigClassName) (
        "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
        "superType" -> strLit(sig.superClass.toString),
        "interfaces" -> array(sig.interfaces.map(cts => strLit(cts.toString)))
      )
    }

    private def methodSignatureAnnotation (sig: JMethodSignature): JavaAnnotation = {
      mkAnnotation(methodSigClassName) (
        "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
        "returnType" -> strLit(sig.returnType.toString),
        "parameters" -> array(sig.parameters.map(p => strLit(p.toString))),
        "throwsTypes" -> array(sig.throwTypes.map(s => strLit(s.toString))),
        "activates" -> array(sig.activates.map(s => strLit(s.toString))),
        "deactivates" -> array(sig.deactivates.map(s => strLit(s.toString))),
        "requires" -> array(sig.requires.map(s => strLit(s.toString)))
      )
    }

    private def fieldSignatureAnnotation (sig: JTypeSignature): JavaAnnotation = {
      mkAnnotation(fieldSigClassName) ("value" -> strLit(sig.toString))
    }

    private def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): JavaAnnotation = {
      mkAnnotation(dslClassName) (
        "priorities" -> array(declaredPriorities.map(p => strLit(p.name)).toList),
        "constraints" -> array(priorityConstraints.map(constraintAnnotation).map(elementAnnotation)),
        "with" -> array(withDSLs.map(classLit))
      )
    }

    private def contextAnnotation: JavaAnnotation = mkAnnotation(contextClassName)()

    private def operatorAnnotation (syntax: JSyntaxDef): JavaAnnotation = syntax match {
      case JExpressionSyntaxDef(priority, pattern) => operatorAnnotation("Expression", priority, pattern)
      case JLiteralSyntaxDef(priority, pattern)    => operatorAnnotation("Literal", priority, pattern)
      case JStatementSyntaxDef(priority, pattern)  => operatorAnnotation("Statement", priority, pattern)
    }

    private def operatorAnnotation (level: String, priority: JPriority, pattern: List[JSyntaxElementDef]): JavaAnnotation = {
      mkAnnotation(operatorClassName) (
        "level" -> enumConst(opLevelClassName, level),
        "priority" -> elementAnnotation(priorityAnnotation(priority)),
        "pattern" -> array(pattern.map(operatorElementAnnotation).map(elementAnnotation))
      )
    }

    private def metaParameterAnnotation (fmp: FormalMetaParameter): JavaAnnotation = {
      mkAnnotation(metaParamClassName)(
        "name" -> strLit(fmp.name),
        "type" -> strLit(fmp.metaType.toString),
        "priority" -> array(fmp.priority.map(priorityAnnotation).map(elementAnnotation).toList),
        "bounds" -> array(fmp.bounds.map(sig => strLit(sig.toString))))
    }

    private def constraintAnnotation (constraint: List[JPriority]): JavaAnnotation = {
      mkAnnotation(constraintClassName)("value" -> array(constraint.map(priorityAnnotation).map(elementAnnotation)))
    }

    private def priorityAnnotation (priority: JPriority): JavaAnnotation = {
      mkAnnotation(priorityClassName)("dsl" -> strLit(priority.clazz.toString), "name" -> strLit(priority.name))
    }

    private def operatorElementAnnotation (elem: JSyntaxElementDef): JavaAnnotation = elem match {
      case JOperatorNameDef(name) => operatorElementAnnotation("Name", name)
      case JOperandDef            => operatorElementAnnotation("Hole", "")
      case JRepetition0Def        => operatorElementAnnotation("Star", "")
      case JRepetition1Def        => operatorElementAnnotation("Plus", "")
      case JOptionalOperandDef    => operatorElementAnnotation("Optional", "")
      case JAndPredicateDef(sig)  => operatorElementAnnotation("AndPredicate", sig.toString)
      case JNotPredicateDef(sig)  => operatorElementAnnotation("NotPredicate", sig.toString)
      case JMetaValueRefDef(name) => operatorElementAnnotation("Reference", name)
    }

    private def operatorElementAnnotation (name: String, value: String): JavaAnnotation = {
      mkAnnotation(opElemClassName) ( "kind" -> enumConst(opElemTypeClassName, name), name -> strLit(value) )
    }

    private def mkAnnotation (annName: String)(args: (String, AnnotationElement)*): JavaAnnotation = annotation(annName, args.toMap)

    private def elementAnnotation (ann: JavaAnnotation) = Union[AnnotationElement](ann)

    private def array (es: List[AnnotationElement]) = Union[AnnotationElement](elementArray(es))

    private def enumConst (enum: String, const: String) = Union[AnnotationElement](enumConstRef(enum, const))

    private def strLit (str: String) = Union[AnnotationElement](Union[JavaLiteral](stringLiteral(str)))

    private def classLit (clazz: JClass) = Union[AnnotationElement](Union[JavaLiteral](classLiteral(clazz)))
  }
}

