package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._

import phenan.util.EitherUtil._

import CommonNames._

class JavaRepr (compiler: JCompiler) {

  class JavaFileRepr (file: IRFile) {
    def packageName = file.packageName.map(_.names.mkString("."))
    def importedClasses = file.importedClassNames.map(_.names.mkString("."))
    def importedPackages = file.importedPackageNames.map(_.names.mkString("."))
    def modules = file.topLevelModules.map(JavaModuleRepr(_))
  }

  type JavaClassMemberRepr = JavaFieldRepr | JavaMethodRepr | JavaConstructorRepr | JavaInstanceInitializerRepr | JavaStaticInitializerRepr | JavaModuleRepr

  type JavaModuleRepr = JavaClassRepr | JavaEnumRepr | JavaInterfaceRepr | JavaDSLRepr

  object JavaModuleRepr {
    def apply (clazz: IRModule): JavaModuleRepr = clazz match {
      case cls: IRTopLevelClass     => new JavaClassRepr(cls).l.l.l
      case enm: IRTopLevelEnum      => new JavaEnumRepr(enm).r.l.l
      case ifc: IRTopLevelInterface => new JavaInterfaceRepr(ifc).r.l
      case dsl: IRTopLevelDSL       => new JavaDSLRepr(dsl).r
    }
  }

  class JavaClassRepr (clazz: IRClass) {
    def annotations = JavaAnnotationsRepr.javaClassAnnotations(clazz)
    def modifiers = clazz.mod
    def name = clazz.simpleName
    def typeParameters = clazz.signature.metaParams.filter(_.metaType == JTypeSignature.typeTypeSig).map(new JavaTypeParamRepr(_))
    def superType = new JavaSignatureRepr(clazz.signature.superClass)
    def interfaces = clazz.signature.interfaces.map(new JavaSignatureRepr(_))
    def members: List[JavaClassMemberRepr] = clazz.declaredMembers.map {
      case field: IRClassField             => new JavaFieldRepr(field).l.l.l.l.l
      case method: IRClassMethod           => new JavaMethodRepr(method).r.l.l.l.l
      case constructor: IRClassConstructor => new JavaConstructorRepr(constructor).r.l.l.l
      case iin: IRClassInstanceInitializer => new JavaInstanceInitializerRepr(iin).r.l.l
      case sin: IRClassStaticInitializer   => new JavaStaticInitializerRepr(sin).r.l
      case module: IRModule                => JavaModuleRepr(module).r
    }
  }

  class JavaEnumRepr (clazz: IREnum) {

  }

  class JavaInterfaceRepr (clazz: IRInterface) {

  }

  class JavaDSLRepr (clazz: IRDSL) {

  }

  class JavaFieldRepr (field: IRField) {

    def modifiers = field.mod
    def fieldType = new JavaSignatureRepr(field.signature)
    def name = field.name

  }

  class JavaMethodRepr (method: IRMethod) {

  }

  class JavaConstructorRepr (constructor: IRConstructor) {

  }

  class JavaInstanceInitializerRepr (instanceInitializer: IRInstanceInitializer) {

  }

  class JavaStaticInitializerRepr (staticInitializer: IRStaticInitializer) {

  }

  class JavaTypeParamRepr (mp: FormalMetaParameter) {
    def name = mp.name
    def bounds = mp.bounds.map(new JavaSignatureRepr(_))
  }

  class JavaSignatureRepr (signature: JTypeSignature) {
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

  class JavaTypeRepr (t: JType)

  /* literals */

  type JavaLiteralRepr = JavaClassLiteralRepr | JavaStringLiteralRepr | JavaCharLiteralRepr | JavaIntLiteralRepr | JavaLongLiteralRepr | JavaBooleanLiteralRepr

  object JavaLiteralRepr {
    def apply (literal: IRJavaLiteral): JavaLiteralRepr = literal match {
      case c: IRClassLiteral   => JavaClassLiteralRepr(c).l.l.l.l.l
      case s: IRStringLiteral  => JavaStringLiteralRepr(s).r.l.l.l.l
      case c: IRCharLiteral    => new JavaCharLiteralRepr(c).r.l.l.l
      case i: IRIntLiteral     => new JavaIntLiteralRepr(i).r.l.l
      case j: IRLongLiteral    => new JavaLongLiteralRepr(j).r.l
      case z: IRBooleanLiteral => new JavaBooleanLiteralRepr(z).r
    }
    def apply (clazz: JavaClassLiteralRepr): JavaLiteralRepr = clazz.l.l.l.l.l
    def apply (s: JavaStringLiteralRepr): JavaLiteralRepr = s.r.l.l.l.l
  }

  trait JavaClassLiteralRepr {
    def className: String
    def dim: Int
  }

  object JavaClassLiteralRepr {
    def apply (c: IRClassLiteral): JavaClassLiteralRepr = c match {
      case IRObjectClassLiteral(clazz, d) => new JavaClassLiteralRepr {
        def className: String = clazz.name
        def dim: Int = d
      }
      case IRPrimitiveClassLiteral(primitive, d) => new JavaClassLiteralRepr {
        def className: String = primitive.name
        def dim: Int = d
      }
    }
    def apply (clazz: JClass): JavaClassLiteralRepr = new JavaClassLiteralRepr {
      def className: String = clazz.name
      def dim: Int = 0
    }
  }

  trait JavaStringLiteralRepr {
    def value: String
  }

  object JavaStringLiteralRepr {
    def apply (literal: IRStringLiteral): JavaStringLiteralRepr = new JavaStringLiteralRepr {
      def value = literal.value
    }
    def apply (v: String): JavaStringLiteralRepr = new JavaStringLiteralRepr {
      def value = v
    }
  }

  class JavaCharLiteralRepr (literal: IRCharLiteral) {
    def value = literal.value
  }

  class JavaIntLiteralRepr (literal: IRIntLiteral) {
    def value = literal.value
  }

  class JavaLongLiteralRepr (literal: IRLongLiteral) {
    def value = literal.value
  }

  class JavaBooleanLiteralRepr (literal: IRBooleanLiteral) {
    def value = literal.value
  }

  /* annotation */

  trait JavaAnnotationRepr {
    def name: String
    def arguments: Map[String, JavaAnnotationElementRepr]
  }

  object JavaAnnotationRepr {
    def apply (ann: IRAnnotation): JavaAnnotationRepr = new JavaAnnotationRepr {
      def name: String = ann.annotationClass.name
      def arguments: Map[String, JavaAnnotationElementRepr] = ann.args.mapValues(JavaAnnotationElementRepr(_))
    }
    def apply (n: String, args: Map[String, JavaAnnotationElementRepr]): JavaAnnotationRepr = new JavaAnnotationRepr {
      def name: String = n
      def arguments: Map[String, JavaAnnotationElementRepr] = args
    }
  }

  type JavaAnnotationElementRepr = JavaAnnElemArrayRepr | JavaAnnotationRepr | JavaLiteralRepr | JavaEnumConstRefRepr

  object JavaAnnotationElementRepr {
    def apply (e: IRAnnotationElement): JavaAnnotationElementRepr = e match {
      case array: IRAnnotationElementArray => JavaAnnElemArrayRepr(array).l.l.l
      case annotation: IRAnnotation => JavaAnnotationRepr(annotation).r.l.l
      case literal: IRJavaLiteral   => JavaLiteralRepr(literal).r.l
      case const: IREnumConstantRef => JavaEnumConstRefRepr(const).r
    }
    def apply (array: JavaAnnElemArrayRepr): JavaAnnotationElementRepr = array.l.l.l
    def apply (ann: JavaAnnotationRepr): JavaAnnotationElementRepr = ann.r.l.l
    def apply (lit: JavaLiteralRepr): JavaAnnotationElementRepr = lit.r.l
    def apply (const: JavaEnumConstRefRepr): JavaAnnotationElementRepr = const.r
  }

  trait JavaAnnElemArrayRepr {
    def elements: List[JavaAnnotationElementRepr]
  }

  object JavaAnnElemArrayRepr {
    def apply (array: IRAnnotationElementArray): JavaAnnElemArrayRepr = new JavaAnnElemArrayRepr {
      def elements = array.array.map(JavaAnnotationElementRepr(_))
    }
    def apply (es: List[JavaAnnotationElementRepr]): JavaAnnElemArrayRepr = new JavaAnnElemArrayRepr {
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
    def javaClassAnnotations (clazz: IRModule): List[JavaAnnotationRepr] = {
      if (clazz.isDSL) except(classSigClassName, dslClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature) :+ dslAnnotation(clazz.declaredPriorities, clazz.priorityConstraints, clazz.withDSLs)
      else except(classSigClassName, dslClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature)
    }

    def javaFieldAnnotations (field: IRField): List[JavaAnnotationRepr] = {
      except(fieldSigClassName)(field.annotations) :+ fieldSignatureAnnotation(field.signature)
    }

    private def except (names: String*)(as: List[IRAnnotation]): List[JavaAnnotationRepr] = as.filterNot { ann => names.contains(ann.annotationClass.internalName) }.map(JavaAnnotationRepr(_))

    private def classSignatureAnnotation (sig: JClassSignature): JavaAnnotationRepr = {
      mkAnnotation(classSigClassName) (
        "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
        "superType" -> strLit(sig.superClass.toString),
        "interfaces" -> array(sig.interfaces.map(cts => strLit(cts.toString)))
      )
    }

    private def methodSignatureAnnotation (sig: JMethodSignature): JavaAnnotationRepr = {
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

    private def fieldSignatureAnnotation (sig: JTypeSignature): JavaAnnotationRepr = {
      mkAnnotation(fieldSigClassName) ("value" -> strLit(sig.toString))
    }

    private def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): JavaAnnotationRepr = {
      mkAnnotation(dslClassName) (
        "priorities" -> array(declaredPriorities.map(p => strLit(p.name)).toList),
        "constraints" -> array(priorityConstraints.map(constraintAnnotation).map(elementAnnotation)),
        "with" -> array(withDSLs.map(classLit))
      )
    }

    private def operatorAnnotation (syntax: JSyntaxDef): JavaAnnotationRepr = syntax match {
      case JExpressionSyntaxDef(priority, pattern) => operatorAnnotation("Expression", priority, pattern)
      case JLiteralSyntaxDef(priority, pattern)    => operatorAnnotation("Literal", priority, pattern)
      case JStatementSyntaxDef(priority, pattern)  => operatorAnnotation("Statement", priority, pattern)
    }

    private def operatorAnnotation (level: String, priority: JPriority, pattern: List[JSyntaxElementDef]): JavaAnnotationRepr = {
      mkAnnotation(operatorClassName) (
        "level" -> enumConst(opLevelClassName, level),
        "priority" -> elementAnnotation(priorityAnnotation(priority)),
        "pattern" -> array(pattern.map(operatorElementAnnotation).map(elementAnnotation))
      )
    }

    private def metaParameterAnnotation (fmp: FormalMetaParameter): JavaAnnotationRepr = {
      mkAnnotation(metaParamClassName)(
        "name" -> strLit(fmp.name),
        "type" -> strLit(fmp.metaType.toString),
        "priority" -> array(fmp.priority.map(priorityAnnotation).map(elementAnnotation).toList),
        "bounds" -> array(fmp.bounds.map(sig => strLit(sig.toString))))
    }

    private def constraintAnnotation (constraint: List[JPriority]): JavaAnnotationRepr = {
      mkAnnotation(constraintClassName)("value" -> array(constraint.map(priorityAnnotation).map(elementAnnotation)))
    }

    private def priorityAnnotation (priority: JPriority): JavaAnnotationRepr = {
      mkAnnotation(priorityClassName)("dsl" -> strLit(priority.clazz.toString), "name" -> strLit(priority.name))
    }

    private def operatorElementAnnotation (elem: JSyntaxElementDef): JavaAnnotationRepr = elem match {
      case JOperatorNameDef(name) => operatorElementAnnotation("Name", name)
      case JOperandDef            => operatorElementAnnotation("Hole", "")
      case JRepetition0Def        => operatorElementAnnotation("Star", "")
      case JRepetition1Def        => operatorElementAnnotation("Plus", "")
      case JOptionalOperandDef    => operatorElementAnnotation("Optional", "")
      case JAndPredicateDef(sig)  => operatorElementAnnotation("AndPredicate", sig.toString)
      case JNotPredicateDef(sig)  => operatorElementAnnotation("NotPredicate", sig.toString)
      case JMetaValueRefDef(name) => operatorElementAnnotation("Reference", name)
    }

    private def operatorElementAnnotation (name: String, value: String): JavaAnnotationRepr = {
      mkAnnotation(opElemClassName) ( "kind" -> enumConst(opElemTypeClassName, name), name -> strLit(value) )
    }

    private def mkAnnotation (annName: String)(args: (String, JavaAnnotationElementRepr)*): JavaAnnotationRepr = JavaAnnotationRepr(annName, args.toMap)

    private def elementAnnotation (ann: JavaAnnotationRepr) = JavaAnnotationElementRepr(ann)

    private def array (es: List[JavaAnnotationElementRepr]) = JavaAnnotationElementRepr(JavaAnnElemArrayRepr(es))

    private def enumConst (enum: String, const: String) = JavaAnnotationElementRepr(JavaEnumConstRefRepr(enum, const))

    private def strLit (str: String) = JavaAnnotationElementRepr(JavaLiteralRepr(JavaStringLiteralRepr(str)))

    private def classLit (clazz: JClass) = JavaAnnotationElementRepr(JavaLiteralRepr(JavaClassLiteralRepr(clazz)))
  }
}

