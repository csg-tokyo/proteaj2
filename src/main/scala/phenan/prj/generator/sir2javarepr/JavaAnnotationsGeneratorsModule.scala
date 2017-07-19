package phenan.prj.generator.sir2javarepr

import phenan.prj.CommonNames._
import phenan.prj._
import phenan.prj.generator.JavaRepr._
import phenan.prj.generator.SimplifiedIRs
import phenan.util._

/**
  * Created by ichikawa on 2017/07/13.
  */
trait JavaAnnotationsGeneratorsModule {
  this: JavaLiteralsGeneratorsModule with SimplifiedIRs with JErasedTypes =>

  object JavaAnnotationsGenerators {
    def classAnnotations (clazz: SIRClass): List[JavaAnnotation] = {
      moduleAnnotations(clazz) :+ dslAnnotation(clazz.declaredPriorities, clazz.priorityConstraints, clazz.withDSLs)
    }

    def enumAnnotations (enum: SIREnum): List[JavaAnnotation] = moduleAnnotations(enum)

    def interfaceAnnotations (interface: SIRInterface): List[JavaAnnotation] = moduleAnnotations(interface)

    def fieldAnnotations (field: SIRField): List[JavaAnnotation] = {
      except(fieldSigClassName)(field.annotations) :+ fieldSignatureAnnotation(field.signature)
    }

    def methodAnnotations (method: SIRMethod): List[JavaAnnotation] = {
      procedureAnnotations(method) ++ method.syntax.map(operatorAnnotation)
    }

    def constructorAnnotations (constructor: SIRConstructor): List[JavaAnnotation] = procedureAnnotations(constructor)

    def moduleAnnotations (clazz: SIRModule): List[JavaAnnotation] = {
      except(classSigClassName, dslClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature)
    }

    def procedureAnnotations (method: SIRProcedure): List[JavaAnnotation] = {
      except(methodSigClassName, operatorClassName)(method.annotations) :+ methodSignatureAnnotation(method.signature)
    }

    def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): JavaAnnotation = {
      mkAnnotation(dslClassName) (
        "priorities"  -> array(declaredPriorities.map(p => strLit(p.name)).toList),
        "constraints" -> array(priorityConstraints.map(constraintAnnotation).map(elementAnnotation)),
        "with"        -> array(withDSLs.map(classLit))
      )
    }

    def operatorAnnotation (syntax: JSyntaxDef): JavaAnnotation = syntax match {
      case JExpressionSyntaxDef(priority, pattern) => operatorAnnotation("Expression", priority, pattern)
      case JLiteralSyntaxDef(priority, pattern)    => operatorAnnotation("Literal", priority, pattern)
      case JStatementSyntaxDef(priority, pattern)  => operatorAnnotation("Statement", priority, pattern)
    }

    def constraintAnnotation (constraint: List[JPriority]): JavaAnnotation = {
      mkAnnotation(constraintClassName)("value" -> array(constraint.map(priorityAnnotation).map(elementAnnotation)))
    }

    def priorityAnnotation (priority: JPriority): JavaAnnotation = mkAnnotation(priorityClassName)(
      "dsl"  -> strLit(typeSigString(priority.clazz)),
      "name" -> strLit(priority.name)
    )

    def operatorAnnotation (level: String, priority: JPriority, pattern: List[JSyntaxElementDef]): JavaAnnotation = {
      mkAnnotation(operatorClassName) (
        "level"    -> enumConst(opLevelClassName, level),
        "priority" -> elementAnnotation(priorityAnnotation(priority)),
        "pattern"  -> array(pattern.map(operatorElementAnnotation).map(elementAnnotation))
      )
    }

    def operatorElementAnnotation (elem: JSyntaxElementDef): JavaAnnotation = elem match {
      case JOperatorNameDef(name)    => operatorElementAnnotation("Name", name, None)
      case JRegexNameDef(name)       => operatorElementAnnotation("Regex", name, None)
      case JOperandDef(p)            => operatorElementAnnotation("Hole", "", p)
      case JRepetition0Def(p)        => operatorElementAnnotation("Star", "", p)
      case JRepetition1Def(p)        => operatorElementAnnotation("Plus", "", p)
      case JOptionalOperandDef(p)    => operatorElementAnnotation("Optional", "", p)
      case JAndPredicateDef(sig, p)  => operatorElementAnnotation("AndPredicate", typeSigString(sig), p)
      case JNotPredicateDef(sig, p)  => operatorElementAnnotation("NotPredicate", typeSigString(sig), p)
      case JMetaValueRefDef(name, p) => operatorElementAnnotation("Reference", name, p)
    }

    def operatorElementAnnotation (name: String, value: String, priority: Option[JPriority]): JavaAnnotation = mkAnnotation(opElemClassName) (
      "kind"     -> enumConst(opElemTypeClassName, name),
      "name"     -> strLit(value),
      "priority" -> array(priority.map(priorityAnnotation).map(elementAnnotation).toList)
    )



    def classSignatureAnnotation (sig: JClassSignature): JavaAnnotation = mkAnnotation(classSigClassName) (
      "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
      "superType"      -> strLit(typeSigString(sig.superClass)),
      "interfaces"     -> array(sig.interfaces.map(cts => strLit(typeSigString(cts))))
    )

    def methodSignatureAnnotation (sig: JMethodSignature): JavaAnnotation = mkAnnotation(methodSigClassName) (
      "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
      "returnType"     -> strLit(typeSigString(sig.returnType)),
      "returnBounds"   -> array(sig.returnBounds.map(s => strLit(typeSigString(s)))),
      "parameters"     -> array(sig.parameters.map(p => strLit(parameterSigString(p)))),
      "throwsTypes"    -> array(sig.throwTypes.map(s => strLit(typeSigString(s)))),
      "activates"      -> array(sig.activates.map(s => strLit(typeSigString(s)))),
      "deactivates"    -> array(sig.deactivates.map(s => strLit(typeSigString(s)))),
      "requires"       -> array(sig.requires.map(s => strLit(typeSigString(s))))
    )

    def fieldSignatureAnnotation (sig: JTypeSignature): JavaAnnotation = {
      mkAnnotation(fieldSigClassName) ("value" -> strLit(typeSigString(sig)))
    }

    def metaParameterAnnotation (fmp: FormalMetaParameter): JavaAnnotation = mkAnnotation(metaParamClassName)(
      "name"   -> strLit(fmp.name),
      "type"   -> strLit(typeSigString(fmp.metaType)),
      "bounds" -> array(fmp.bounds.map(sig => strLit(typeSigString(sig))))
    )

    def annotation (ann: SIRAnnotation): JavaAnnotation = JavaAnnotation (ann.annotationClass.name, ann.arguments.mapValues(annotationElement))

    def annotationElement (e: SIRAnnotationElement): AnnotationElement = e match {
      case array: SIRAnnotationElementArray => Union[AnnotationElement](elementArray(array))
      case ann: SIRAnnotation               => Union[AnnotationElement](annotation(ann))
      case literal: SIRJavaLiteral          => Union[AnnotationElement](JavaLiteralsGenerators.javaLiteral(literal))
      case const: SIREnumConstantRef        => Union[AnnotationElement](enumConstRef(const))
    }

    def elementArray (array: SIRAnnotationElementArray): ElementArray = ElementArray (array.components.map(annotationElement))

    def enumConstRef (const: SIREnumConstantRef): EnumConstRef = EnumConstRef (const.field.declaringClass.name, const.field.name)

    private def mkAnnotation (annName: String)(args: (String, AnnotationElement)*): JavaAnnotation = JavaAnnotation(annName.replace('/', '.'), args.toMap)

    private def except (names: String*)(as: List[SIRAnnotation]): List[JavaAnnotation] = as.filterNot { ann => names.contains(ann.annotationClass.internalName) }.map(annotation)

    private def elementAnnotation (ann: JavaAnnotation) = Union[AnnotationElement](ann)

    private def array (es: List[AnnotationElement]) = Union[AnnotationElement](ElementArray(es))

    private def enumConst (enum: String, const: String) = Union[AnnotationElement](EnumConstRef(enum.replace('/', '.'), const))

    private def strLit (str: String) = Union[AnnotationElement](Union[JavaLiteral](Literal(str)))

    private def classLit (clazz: JClass) = Union[AnnotationElement](Union[JavaLiteral](JavaLiteralsGenerators.classLiteral(clazz)))


    /* signature -> string */

    private def parameterSigString (sig: JParameterSignature): String = {
      val cs = sig.contexts.map('@' + typeSigString(_)).mkString
      val da = sig.defaultArg.map('?' + _).mkString
      val sc = sig.scopes.map('#' + typeSigString(_)).mkString
      if(sig.varArgs) cs + typeSigString(sig.typeSig) + '*' + da + sc
      else cs + typeSigString(sig.typeSig) + da + sc
    }

    private def typeArgString (sig: JTypeArgument): String = sig match {
      case typeSig: JTypeSignature        => typeSigString(typeSig)
      case WildcardArgument(upper, lower) => upper.map('+' + typeSigString(_)) orElse lower.map('-' + typeSigString(_)) getOrElse "*"
      case MetaVariableSignature(name)    => s"P$name;"
    }

    private def typeSigString (sig: JTypeSignature): String = sig match {
      case JArrayTypeSignature(component) => s"[${typeSigString(component)}"
      case JTypeVariableSignature(name)   => s"T$name;"
      case classTypeSig: JClassTypeSignature     => s"L${classTypeSignatureStringBody(classTypeSig)};"
      case primitiveSig: JPrimitiveTypeSignature => primitiveTypeSignatureString(primitiveSig)
      case JCapturedWildcardSignature(_, _)      =>
        throw new RuntimeException("captured wildcard is found in the type signature")
    }

    private def classTypeSignatureStringBody (sig: JClassTypeSignature): String = sig match {
      case SimpleClassTypeSignature(clazz, args) =>
        if (args.isEmpty) clazz
        else clazz + args.map(typeArgString).mkString("<", "", ">")
      case MemberClassTypeSignature(outer, clazz, args) =>
        if (args.isEmpty) classTypeSignatureStringBody(outer) + '.' + clazz
        else classTypeSignatureStringBody(outer) + '.' + clazz + args.map(typeArgString).mkString("<", "", ">")
    }

    private def primitiveTypeSignatureString (sig: JPrimitiveTypeSignature): String = sig match {
      case ByteTypeSignature   => "B"
      case CharTypeSignature   => "C"
      case DoubleTypeSignature => "D"
      case FloatTypeSignature  => "F"
      case IntTypeSignature    => "I"
      case LongTypeSignature   => "J"
      case ShortTypeSignature  => "S"
      case BoolTypeSignature   => "Z"
      case VoidTypeSignature   => "V"
    }
  }
}
