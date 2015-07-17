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

  private def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.dslClassName) (
      "priorities" -> array(declaredPriorities.map(p => strLit(p.name)).toList),
      "constraints" -> ???,
      "with" -> ???
    )
  }

  private def metaParameterAnnotation (fmp: FormalMetaParameter): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.metaParamClassName)(
      "name" -> strLit(fmp.name),
      "type" -> strLit(fmp.metaType.toString),
      "priority" -> array(fmp.priority.flatMap(priorityAnnotation).toList),
      "bounds" -> array(fmp.bounds.map(sig => strLit(sig.toString))))
  }

  private def priorityAnnotation (priority: JPriority): Option[IRAnnotation] = {
    mkAnnotation(CommonNames.priorityClassName)("dsl" -> strLit(priority.clazz.toString), "name" -> strLit(priority.name))
  }

  private def mkAnnotation (annName: String)(args: (String, IRAnnotationElement)*): Option[IRAnnotation] = {
    compiler.classLoader.loadClass_PE(annName).map(IRAnnotation(_, args.toMap))
  }

  private def array (e: List[IRAnnotationElement]): IRAnnotationElementArray = IRAnnotationElementArray(e)

  private def strLit (str: String) = IRStringLiteral(str, compiler)

  private def classLit (sig: JClassTypeSignature): IRObjectClassLiteral = ???
}