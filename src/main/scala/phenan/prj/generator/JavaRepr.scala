package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._

/**
 * Created by ichikawa on 15/07/17.
 */
class JavaRepr {

}

trait JavaAnnotations {
  def compiler: JCompiler

  /* for code generation */

  def javaAnnotations: List[IRAnnotation] = {
    ???

    // file.annotationReader.exceptClassAnnotation(annotations)
  }

  private def classSignatureAnnotation (sig: JClassSignature): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.classSigClassName) (
      "metaParameters" -> array(sig.metaParams.flatMap(metaParameterAnnotation)),
      "superType" -> strLit(sig.superClass.toString),
      "interfaces" -> array(sig.interfaces.map(cts => strLit(cts.toString)))
    )
  }

  private def methodSignatureAnnotation (sig: JMethodSignature): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.methodSigClassName) (
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
    mkAnnotation(CommonNames.fieldSigClassName) ("value" -> strLit(sig.toString))
  }

  private def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.dslClassName) (
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
    lvl <- enumConst(CommonNames.opLevelClassName, level)
    pri <- priorityAnnotation(priority)
    ann <- mkAnnotation(CommonNames.operatorClassName) ("level" -> lvl, "priority" -> pri, "pattern" -> array(pattern.flatMap(e => operatorElementAnnotation(e))))
  } yield ann

  private def metaParameterAnnotation (fmp: FormalMetaParameter): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.metaParamClassName)(
      "name" -> strLit(fmp.name),
      "type" -> strLit(fmp.metaType.toString),
      "priority" -> array(fmp.priority.flatMap(priorityAnnotation).toList),
      "bounds" -> array(fmp.bounds.map(sig => strLit(sig.toString))))
  }

  private def constraintAnnotation (constraint: List[JPriority]): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.constraintClassName)("value" -> array(constraint.flatMap(priorityAnnotation)))
  }

  private def priorityAnnotation (priority: JPriority): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.priorityClassName)("dsl" -> strLit(priority.clazz.toString), "name" -> strLit(priority.name))
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
    enumConst(CommonNames.opElemTypeClassName, name).flatMap { kind =>
      mkAnnotation(CommonNames.opElemClassName)("kind" -> kind, name -> strLit(value))
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