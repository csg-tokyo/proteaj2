package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._

import phenan.util._

import CommonNames._

class JavaRepr (compiler: JCompiler) {
  
  class JavaFileRepr (file: IRFile) {
    def packageName = file.packageName.map(_.names.mkString("."))
    def importedClasses = file.importedClassNames.map(_.names.mkString("."))
    def importedPackages = file.importedPackageNames.map(_.names.mkString("."))
    def modules = file.topLevelModules.map(ModuleDef(_))
  }

  type ClassMember = JavaFieldRepr :|: JavaMethodRepr :|: JavaConstructorRepr :|: JavaInstanceInitializerRepr :|: JavaStaticInitializerRepr :|: ModuleDef :|: UNil

  object ClassMember {
    def apply (member: IRClassMember): ClassMember = member match {
      case field: IRClassField             => Union[ClassMember](new JavaFieldRepr(field))
      case method: IRClassMethod           => Union[ClassMember](JavaMethodRepr(method))
      case constructor: IRClassConstructor => Union[ClassMember](new JavaConstructorRepr(constructor))
      case iin: IRClassInstanceInitializer => Union[ClassMember](new JavaInstanceInitializerRepr(iin))
      case sin: IRClassStaticInitializer   => Union[ClassMember](new JavaStaticInitializerRepr(sin))
      case module: IRModule                => Union[ClassMember](ModuleDef(module))
    }
    def apply (synthetic: IRSyntheticMethod): ClassMember = Union[ClassMember](JavaMethodRepr(synthetic))
  }

  type ModuleDef = ClassDef :|: JavaEnumRepr :|: JavaInterfaceRepr :|: UNil

  object ModuleDef {
    def apply (clazz: IRModule): ModuleDef = clazz match {
      case cls: IRTopLevelClass     => Union[ModuleDef](ClassDef(cls))
      case enm: IRTopLevelEnum      => Union[ModuleDef](new JavaEnumRepr(enm))
      case ifc: IRTopLevelInterface => Union[ModuleDef](new JavaInterfaceRepr(ifc))
      case dsl: IRTopLevelDSL       => Union[ModuleDef](ClassDef(dsl))
    }
  }

  trait ClassDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def typeParameters: List[JavaTypeParamRepr]
    def superType: JavaSignatureRepr
    def interfaces: List[JavaSignatureRepr]
    def members: List[ClassMember]
  }

  object ClassDef {
    def apply (clazz: IRClass): ClassDef = new ClassDef {
      def annotations = JavaAnnotationsRepr.javaClassAnnotations(clazz)
      def modifiers = clazz.mod
      def name = clazz.simpleName
      def typeParameters = clazz.signature.metaParams.filter(_.metaType == JTypeSignature.typeTypeSig).map(new JavaTypeParamRepr(_))
      def superType = JavaSignatureRepr(clazz.signature.superClass)
      def interfaces = clazz.signature.interfaces.map(JavaSignatureRepr(_))
      def members: List[ClassMember] = clazz.declaredMembers.map(ClassMember(_)) ++ clazz.syntheticMethods.map(ClassMember(_))
    }
    def apply (dsl: IRDSL): ClassDef = new ClassDef {
      def annotations: List[JavaAnnotation] = ???
      def modifiers: JModifier = dsl.mod
      def name: String = dsl.simpleName
      def typeParameters: List[JavaTypeParamRepr] = Nil
      def superType: JavaSignatureRepr = ???
      def interfaces: List[JavaSignatureRepr] = Nil
      def members: List[ClassMember] = ???

    }
  }

  class JavaEnumRepr (clazz: IREnum) {

  }

  class JavaInterfaceRepr (clazz: IRInterface) {

  }

  class JavaFieldRepr (field: IRField) {

    def modifiers = field.mod
    def fieldType = JavaSignatureRepr(field.signature)
    def name = field.name

  }

  trait JavaMethodRepr {

  }

  object JavaMethodRepr {
    def apply (method: IRMethod): JavaMethodRepr = new JavaMethodRepr {

    }
    def apply (synthetic: IRSyntheticMethod): JavaMethodRepr = new JavaMethodRepr {

    }
  }

  class JavaConstructorRepr (constructor: IRConstructor) {

  }

  class JavaInstanceInitializerRepr (instanceInitializer: IRInstanceInitializer) {

  }

  class JavaStaticInitializerRepr (staticInitializer: IRStaticInitializer) {

  }

  class JavaTypeParamRepr (mp: FormalMetaParameter) {
    def name = mp.name
    def bounds = mp.bounds.map(JavaSignatureRepr(_))
  }


  type JavaTypeArgumentRepr = JavaSignatureRepr :|: JavaWildcardRepr :|: UNil

  type JavaWildcardRepr = JavaUnboundWildcardRepr.type :|: JavaUpperBoundWildcardRepr :|: JavaLowerBoundWildcardRepr :|: UNil

  type JavaSignatureRepr = JavaClassSigRepr :|: JavaArraySigRepr :|: JavaTypeVariableSigRepr :|: JavaPrimitiveSigRepr :|: UNil

  type JavaClassSigRepr = JavaTopLevelClassSigRepr :|: JavaMemberClassSigRepr :|: UNil

  object JavaSignatureRepr {
    def apply (signature: JTypeSignature): JavaSignatureRepr = {
      ???
    }
  }

  class JavaTopLevelClassSigRepr ()

  class JavaMemberClassSigRepr ()

  class JavaArraySigRepr ()

  class JavaTypeVariableSigRepr ()

  class JavaPrimitiveSigRepr ()

  object JavaUnboundWildcardRepr

  class JavaUpperBoundWildcardRepr ()

  class JavaLowerBoundWildcardRepr ()


  /*class JavaSignatureRepr (signature: JTypeSignature) {
    def typeName = javaTypeName(signature)

    private def javaTypeName (signature: JTypeSignature): String = signature match {
      case SimpleClassTypeSignature(clazz, Nil)         => clazz.replace('/', '.')
      case SimpleClassTypeSignature(clazz, args)        => clazz.replace('/', '.') + args.map(javaTypeArgumentName).mkString("<", ",", ">")
      case MemberClassTypeSignature(outer, clazz, Nil)  => javaTypeName(outer) + '.' + clazz
      case MemberClassTypeSignature(outer, clazz, args) => javaTypeName(outer) + '.' + clazz + args.map(javaTypeArgumentName).mkString("<", ",", ">")
      case JArrayTypeSignature(component)               => javaTypeName(component) + "[]"
      case JTypeVariableSignature(name)                 => name
      case primitive: JPrimitiveTypeSignature           => javaPrimitiveTypeName(primitive)
      case cap: JCapturedWildcardSignature              => compiler.state.errorAndReturn("captured wildcard is found in generated Java code", "#captured#")
    }

    private def javaPrimitiveTypeName (primitive: JPrimitiveTypeSignature): String = primitive match {
      case ByteTypeSignature   => "byte"
      case CharTypeSignature   => "char"
      case DoubleTypeSignature => "double"
      case FloatTypeSignature  => "float"
      case IntTypeSignature    => "int"
      case LongTypeSignature   => "long"
      case ShortTypeSignature  => "short"
      case BoolTypeSignature   => "boolean"
      case VoidTypeSignature   => "void"
    }

    private def javaTypeArgumentName (arg: JTypeArgument): Option[String] = arg match {
      case signature: JTypeSignature     => Some(javaTypeName(signature))
      case WildcardArgument(Some(ub), _) => Some("? extends " + javaTypeName(ub))
      case WildcardArgument(_, Some(lb)) => Some("? super " + javaTypeName(lb))
      case WildcardArgument(None, None)  => Some("?")
      case MetaVariableSignature(name)   => None
    }
  }
  */
  class JavaTypeRepr (t: JType)

  /* literals */

  type JavaLiteral = ClassLiteral :|: Literal[String] :|: Literal[Char] :|: Literal[Int] :|: Literal[Long] :|: Literal[Boolean] :|: UNil

  object JavaLiteral {
    def apply (literal: IRJavaLiteral): JavaLiteral = literal match {
      case c: IRClassLiteral   => Union[JavaLiteral](ClassLiteral(c))
      case s: IRStringLiteral  => Union[JavaLiteral](Literal(s))
      case c: IRCharLiteral    => Union[JavaLiteral](Literal(c))
      case i: IRIntLiteral     => Union[JavaLiteral](Literal(i))
      case j: IRLongLiteral    => Union[JavaLiteral](Literal(j))
      case z: IRBooleanLiteral => Union[JavaLiteral](Literal(z))
    }
  }

  trait ClassLiteral {
    def className: String
    def dim: Int
  }

  object ClassLiteral {
    def apply (c: IRClassLiteral): ClassLiteral = c match {
      case IRObjectClassLiteral(clazz, d) => lit(clazz.name, d)
      case IRPrimitiveClassLiteral(primitive, d) => lit(primitive.name, d)
    }
    def apply (clazz: JClass): ClassLiteral = lit(clazz.name, 0)
    private def lit (name: String, dimension: Int): ClassLiteral = new ClassLiteral {
      def className: String = name
      def dim: Int = dimension
    }
  }

  trait Literal[T] {
    def value: T
  }
  
  object Literal {
    def apply (literal: IRStringLiteral): Literal[String] = lit(literal.value)
    def apply (literal: IRCharLiteral): Literal[Char] = lit(literal.value)
    def apply (literal: IRIntLiteral): Literal[Int] = lit(literal.value)
    def apply (literal: IRLongLiteral): Literal[Long] = lit(literal.value)
    def apply (literal: IRBooleanLiteral): Literal[Boolean] = lit(literal.value)
    def apply (v: String): Literal[String] = lit(v)
    private def lit [T] (t: T): Literal[T] = new Literal[T] {
      def value = t
    }
  }

  /* annotation */

  trait JavaAnnotation {
    def name: String
    def arguments: Map[String, AnnotationElement]
  }

  object JavaAnnotation {
    def apply (ann: IRAnnotation): JavaAnnotation = new JavaAnnotation {
      def name: String = ann.annotationClass.name
      def arguments: Map[String, AnnotationElement] = ann.args.mapValues(AnnotationElement(_))
    }
    def apply (n: String, args: Map[String, AnnotationElement]): JavaAnnotation = new JavaAnnotation {
      def name: String = n
      def arguments: Map[String, AnnotationElement] = args
    }
  }

  type AnnotationElement = JavaAnnElemArrayRepr :|: JavaAnnotation :|: JavaLiteral :|: JavaEnumConstRefRepr :|: UNil

  object AnnotationElement {
    def apply (e: IRAnnotationElement): AnnotationElement = e match {
      case array: IRAnnotationElementArray => Union[AnnotationElement](JavaAnnElemArrayRepr(array))
      case annotation: IRAnnotation => Union[AnnotationElement](JavaAnnotation(annotation))
      case literal: IRJavaLiteral   => Union[AnnotationElement](JavaLiteral(literal))
      case const: IREnumConstantRef => Union[AnnotationElement](JavaEnumConstRefRepr(const))
    }
  }

  trait JavaAnnElemArrayRepr {
    def elements: List[AnnotationElement]
  }

  object JavaAnnElemArrayRepr {
    def apply (array: IRAnnotationElementArray): JavaAnnElemArrayRepr = new JavaAnnElemArrayRepr {
      def elements = array.array.map(AnnotationElement(_))
    }
    def apply (es: List[AnnotationElement]): JavaAnnElemArrayRepr = new JavaAnnElemArrayRepr {
      def elements = es
    }
  }

  trait JavaEnumConstRefRepr {
    def enumName: String
    def constantName: String
  }

  object JavaEnumConstRefRepr {
    def apply (const: IREnumConstantRef): JavaEnumConstRefRepr = new JavaEnumConstRefRepr {
      def enumName = const.field.declaringClass.name
      def constantName = const.field.name
    }
    def apply (enum: String, const: String): JavaEnumConstRefRepr = new JavaEnumConstRefRepr {
      def enumName = enum
      def constantName = const
    }
  }

  object JavaAnnotationsRepr {
    def javaClassAnnotations (clazz: IRModule): List[JavaAnnotation] = {
      if (clazz.isDSL) except(classSigClassName, dslClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature) :+ dslAnnotation(clazz.declaredPriorities, clazz.priorityConstraints, clazz.withDSLs)
      else except(classSigClassName, dslClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature)
    }

    def javaFieldAnnotations (field: IRField): List[JavaAnnotation] = {
      except(fieldSigClassName)(field.annotations) :+ fieldSignatureAnnotation(field.signature)
    }

    private def except (names: String*)(as: List[IRAnnotation]): List[JavaAnnotation] = as.filterNot { ann => names.contains(ann.annotationClass.internalName) }.map(JavaAnnotation(_))

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

    private def mkAnnotation (annName: String)(args: (String, AnnotationElement)*): JavaAnnotation = JavaAnnotation(annName, args.toMap)

    private def elementAnnotation (ann: JavaAnnotation) = Union[AnnotationElement](ann)

    private def array (es: List[AnnotationElement]) = Union[AnnotationElement](JavaAnnElemArrayRepr(es))

    private def enumConst (enum: String, const: String) = Union[AnnotationElement](JavaEnumConstRefRepr(enum, const))

    private def strLit (str: String) = Union[AnnotationElement](Union[JavaLiteral](Literal(str)))

    private def classLit (clazz: JClass) = Union[AnnotationElement](Union[JavaLiteral](ClassLiteral(clazz)))
  }
}

