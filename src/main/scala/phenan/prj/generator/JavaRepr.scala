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

  }

  trait InterfaceDef {

  }

  trait FieldDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def fieldType: TypeSig
    def name: String
    def initializer: Option[Expression]
  }

  trait MethodDef {

  }

  trait ConstructorDef {

  }

  trait InstanceInitializerDef {

  }

  trait StaticInitializerDef {

  }

  type Statement = LocalDeclaration :|: SingleStatement :|: UNil

  type SingleStatement = ControlStatement :|: Block :|: ExpressionStatement :|: UNil

  type ControlStatement = ReturnStatement

  trait LocalDeclaration {

  }

  trait ReturnStatement

  trait Block {
    def statements: List[Statement]
  }

  trait ExpressionStatement {
    def expression: Expression
  }

  type Expression = JavaLiteral

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
    def typeParameters = clazz.signature.metaParams.filter(_.metaType == JTypeSignature.typeTypeSig).map(typeParam)
    def superType = classSig(clazz.signature.superClass)
    def interfaces = clazz.signature.interfaces.map(classSig)
    def members: List[ClassMember] = clazz.declaredMembers.map(classMember) ++ clazz.syntheticMethods.map(syntheticMember)
  }

  def enumDef (enum: IREnum): EnumDef = new EnumDef {

  }

  def interfaceDef (interface: IRInterface): InterfaceDef = new InterfaceDef {

  }

  def dslDef (dsl: IRDSL): ClassDef = new ClassDef {
    def annotations: List[JavaAnnotation] = Annotations.dslAnnotations(dsl)
    def modifiers = dsl.mod
    def name = dsl.simpleName
    def typeParameters = Nil
    def superType = objectClassSig
    def interfaces = Nil
    def members: List[ClassMember] = dsl.declaredMembers.flatMap(dslMember) ++ dsl.syntheticMethods.map(syntheticMember)
  }

  def contextDef (context: IRContext): ClassDef = new ClassDef {
    def annotations: List[JavaAnnotation] = Annotations.contextAnnotations(context)
    def modifiers = context.mod
    def name = context.simpleName
    def typeParameters = context.signature.metaParams.filter(_.metaType == JTypeSignature.typeTypeSig).map(typeParam)
    def superType = objectClassSig
    def interfaces = Nil
    def members: List[ClassMember] = context.declaredMembers.map(contextMember) ++ context.syntheticMethods.map(syntheticMember)
  }

  def classMember (member: IRClassMember): ClassMember = member match {
    case field: IRClassField             => Union[ClassMember](fieldDef(field))
    case method: IRClassMethod           => Union[ClassMember](methodDef(method))
    case constructor: IRClassConstructor => Union[ClassMember](constructorDef(constructor))
    case iin: IRClassInstanceInitializer => Union[ClassMember](instanceInitializerDef(iin))
    case sin: IRClassStaticInitializer   => Union[ClassMember](staticInitializerDef(sin))
    case module: IRModule                => Union[ClassMember](moduleDef(module))
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

  def syntheticMember (synthetic: IRSyntheticMethod): ClassMember = Union[ClassMember](syntheticMethodDef(synthetic))

  def fieldDef (field: IRField): FieldDef = new FieldDef {
    def annotations = Annotations.fieldAnnotations(field)
    def modifiers = field.mod
    def fieldType = typeSig(field.signature)
    def name = field.name
    def initializer: Option[Expression] = ???
  }

  def methodDef (method: IRMethod): MethodDef = new MethodDef {

  }

  def operatorDef (operator: IROperator): MethodDef = new MethodDef {

  }

  def syntheticMethodDef (synthetic: IRSyntheticMethod): MethodDef = new MethodDef {

  }

  def constructorDef (constructor: IRConstructor): ConstructorDef = new ConstructorDef {

  }

  def instanceInitializerDef (iin: IRClassInstanceInitializer): InstanceInitializerDef = new InstanceInitializerDef {

  }

  def staticInitializerDef (sin: IRClassStaticInitializer): StaticInitializerDef = new StaticInitializerDef {

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

    def dslAnnotations (dsl: IRDSL): List[JavaAnnotation] = {
      classLikeAnnotations(dsl) :+ dslAnnotation(dsl.declaredPriorities, dsl.priorityConstraints, dsl.withDSLs)
    }

    def contextAnnotations (context: IRContext): List[JavaAnnotation] = {
      classLikeAnnotations(context) :+ contextAnnotation
    }

    def fieldAnnotations (field: IRField): List[JavaAnnotation] = {
      except(fieldSigClassName)(field.annotations) :+ fieldSignatureAnnotation(field.signature)
    }

    private def classLikeAnnotations (clazz: IRModule): List[JavaAnnotation] = {
      except(classSigClassName, dslClassName, contextClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature)
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

