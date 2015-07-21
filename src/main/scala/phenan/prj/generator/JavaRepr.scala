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

  type JavaLiteralRepr = JavaClassLiteralRepr | JavaStringLiteralRepr | JavaCharLiteralRepr | JavaIntLiteralRepr | JavaLongLiteralRepr | JavaBooleanLiteralRepr

  trait JavaClassLiteralRepr {
    def className: String
    def dim: Int
  }

  class JavaStringLiteralRepr (literal: IRStringLiteral)

  class JavaCharLiteralRepr (literal: IRCharLiteral)

  class JavaIntLiteralRepr (literal: IRIntLiteral)

  class JavaLongLiteralRepr (literal: IRLongLiteral)

  class JavaBooleanLiteralRepr (literal: IRBooleanLiteral)

  type JavaAnnotationElementRepr = JavaAnnElemArrayRepr | JavaAnnotationRepr | JavaLiteralRepr | JavaEnumConstRefRepr

  object JavaAnnotationElementRepr {
    def apply (e: IRAnnotationElement): JavaAnnotationElementRepr = ???
  }

  class JavaAnnElemArrayRepr (array: IRAnnotationElementArray) {
    def elements: List[JavaAnnotationElementRepr] = ???
  }

  class JavaEnumConstRefRepr (const: IREnumConstantRef) {

  }



  object JavaAnnotationsRepr {
    def javaClassAnnotations (clazz: IRModule): List[IRAnnotation] = {
      if (clazz.isDSL) except(classSigClassName, dslClassName)(clazz.annotations) ++ classSignatureAnnotation(clazz.signature) ++ dslAnnotation(clazz.declaredPriorities, clazz.priorityConstraints, clazz.withDSLs)
      else except(classSigClassName, dslClassName)(clazz.annotations) ++ classSignatureAnnotation(clazz.signature)
    }

    def javaFieldAnnotations (field: IRField): List[IRAnnotation] = {
      except(fieldSigClassName)(field.annotations) ++ fieldSignatureAnnotation(field.signature)
    }

    private def except (names: String*)(as: List[IRAnnotation]): List[IRAnnotation] = as.filterNot { ann => names.contains(ann.annotationClass.internalName) }

    private def classSignatureAnnotation (sig: JClassSignature): Option[IRAnnotation] = {
      mkAnnotation(classSigClassName) (
        "metaParameters" -> array(sig.metaParams.flatMap(metaParameterAnnotation)),
        "superType" -> strLit(sig.superClass.toString),
        "interfaces" -> array(sig.interfaces.map(cts => strLit(cts.toString)))
      )
    }

    private def methodSignatureAnnotation (sig: JMethodSignature): Option[IRAnnotation] = {
      mkAnnotation(methodSigClassName) (
        "metaParameters" -> array(sig.metaParams.flatMap(metaParameterAnnotation)),
        "returnType" -> strLit(sig.returnType.toString),
        "parameters" -> array(sig.parameters.map(p => strLit(p.toString))),
        "throwsTypes" -> array(sig.throwTypes.map(s => strLit(s.toString))),
        "activates" -> array(sig.activates.map(s => strLit(s.toString))),
        "deactivates" -> array(sig.deactivates.map(s => strLit(s.toString))),
        "requires" -> array(sig.requires.map(s => strLit(s.toString)))
      )
    }

    private def fieldSignatureAnnotation (sig: JTypeSignature): Option[IRAnnotation] = {
      mkAnnotation(fieldSigClassName) ("value" -> strLit(sig.toString))
    }

    private def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): Option[IRAnnotation] = {
      mkAnnotation(dslClassName) (
        "priorities" -> array(declaredPriorities.map(p => strLit(p.name)).toList),
        "constraints" -> array(priorityConstraints.flatMap(constraintAnnotation)),
        "with" -> array(withDSLs.map(classLit))
      )
    }

    private def operatorAnnotation (syntax: JSyntaxDef): Option[IRAnnotation] = syntax match {
      case JExpressionSyntaxDef(priority, pattern) => operatorAnnotation("Expression", priority, pattern)
      case JLiteralSyntaxDef(priority, pattern)    => operatorAnnotation("Literal", priority, pattern)
      case JStatementSyntaxDef(priority, pattern)  => operatorAnnotation("Statement", priority, pattern)
    }

    private def operatorAnnotation (level: String, priority: JPriority, pattern: List[JSyntaxElementDef]): Option[IRAnnotation] = for {
      lvl <- enumConst(opLevelClassName, level)
      pri <- priorityAnnotation(priority)
      ann <- mkAnnotation(operatorClassName) ("level" -> lvl, "priority" -> pri, "pattern" -> array(pattern.flatMap(e => operatorElementAnnotation(e))))
    } yield ann

    private def metaParameterAnnotation (fmp: FormalMetaParameter): Option[IRAnnotation] = {
      mkAnnotation(metaParamClassName)(
        "name" -> strLit(fmp.name),
        "type" -> strLit(fmp.metaType.toString),
        "priority" -> array(fmp.priority.flatMap(priorityAnnotation).toList),
        "bounds" -> array(fmp.bounds.map(sig => strLit(sig.toString))))
    }

    private def constraintAnnotation (constraint: List[JPriority]): Option[IRAnnotation] = {
      mkAnnotation(constraintClassName)("value" -> array(constraint.flatMap(priorityAnnotation)))
    }

    private def priorityAnnotation (priority: JPriority): Option[IRAnnotation] = {
      mkAnnotation(priorityClassName)("dsl" -> strLit(priority.clazz.toString), "name" -> strLit(priority.name))
    }

    private def operatorElementAnnotation (elem: JSyntaxElementDef): Option[IRAnnotation] = elem match {
      case JOperatorNameDef(name) => operatorElementAnnotation("Name", name)
      case JOperandDef            => operatorElementAnnotation("Hole", "")
      case JRepetition0Def        => operatorElementAnnotation("Star", "")
      case JRepetition1Def        => operatorElementAnnotation("Plus", "")
      case JOptionalOperandDef    => operatorElementAnnotation("Optional", "")
      case JAndPredicateDef(sig)  => operatorElementAnnotation("AndPredicate", sig.toString)
      case JNotPredicateDef(sig)  => operatorElementAnnotation("NotPredicate", sig.toString)
      case JMetaValueRefDef(name) => operatorElementAnnotation("Reference", name)
    }

    private def operatorElementAnnotation (name: String, value: String): Option[IRAnnotation] = {
      enumConst(opElemTypeClassName, name).flatMap { kind =>
        mkAnnotation(opElemClassName)("kind" -> kind, name -> strLit(value))
      }
    }

    private def mkAnnotation (annName: String)(args: (String, IRAnnotationElement)*): Option[IRAnnotation] = {
      compiler.classLoader.loadClass_PE(annName).map(IRAnnotation(_, args.toMap))
    }

    private def enumConst (enumName: String, constName: String): Option[IREnumConstantRef] = {
      compiler.classLoader.loadClass_PE(enumName).flatMap(_.fields.find(_.name == constName)).map(IREnumConstantRef)
    }

    private def array (e: List[IRAnnotationElement]): IRAnnotationElementArray = IRAnnotationElementArray(e)

    private def strLit (str: String) = IRStringLiteral(str, compiler)

    private def classLit (sig: JClass): IRObjectClassLiteral = IRObjectClassLiteral(sig, 0)
  }
}

