package phenan.prj.internal

import phenan.prj._
import phenan.prj.signature._
import phenan.prj.state.JState

import scalaz._
import Scalaz._
import Memo._

class AnnotationReader (classFile: BClassFile)(implicit state: JState) {
  import classFile.poolReader._

  def classAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): JClassAnnotations = attributeReader.map(classAnnotations(_, defaultClassAnnotations)) =<< attribute | defaultClassAnnotations
  def methodAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): JMethodAnnotations = attributeReader.map(methodAnnotations(_, defaultMethodAnnotations)) =<< attribute | defaultMethodAnnotations
  def fieldAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): JFieldAnnotations = attributeReader.map(fieldAnnotations(_, defaultFieldAnnotations)) =<< attribute | defaultFieldAnnotations

  private lazy val defaultClassAnnotations = JClassAnnotations(None, None, false, false)
  private lazy val defaultMethodAnnotations = JMethodAnnotations(None, None, false, false)
  private lazy val defaultFieldAnnotations = JFieldAnnotations(None, false)

  private def classAnnotations (as: List[(String, BAnnotation)], result: JClassAnnotations): JClassAnnotations = as match {
    case (CommonNames.classSigClassName, ann) :: rest => classAnnotations(rest, JClassAnnotations(classSignature(ann), result.dsl, result.isPure, result.isContext))
    case (CommonNames.dslClassName, ann) :: rest      => classAnnotations(rest, JClassAnnotations(result.signature, dsl(ann), result.isPure, result.isContext))
    case (CommonNames.pureClassName, _) :: rest       => classAnnotations(rest, JClassAnnotations(result.signature, result.dsl, true, result.isContext))
    case (CommonNames.contextClassName, _) :: rest    => classAnnotations(rest, JClassAnnotations(result.signature, result.dsl, result.isPure, true))
    case (_, ann) :: rest                             => classAnnotations(rest, result)
    case Nil => result
  }

  private def methodAnnotations (as: List[(String, BAnnotation)], result: JMethodAnnotations): JMethodAnnotations = as match {
    case (CommonNames.methodSigClassName, ann) :: rest => methodAnnotations(rest, JMethodAnnotations(methodSignature(ann), result.operator, result.isPure, result.isFinalizer))
    case (CommonNames.operatorClassName, ann) :: rest  => methodAnnotations(rest, JMethodAnnotations(result.signature, operator(ann), result.isPure, result.isFinalizer))
    case (CommonNames.pureClassName, _) :: rest        => methodAnnotations(rest, JMethodAnnotations(result.signature, result.operator, true, result.isFinalizer))
    case (CommonNames.finalizerClassName, _) :: rest   => methodAnnotations(rest, JMethodAnnotations(result.signature, result.operator, result.isPure, true))
    case (_, ann) :: rest                              => methodAnnotations(rest, result)
    case Nil => result
  }

  private def fieldAnnotations (as: List[(String, BAnnotation)], result: JFieldAnnotations): JFieldAnnotations = as match {
    case (CommonNames.fieldSigClassName, ann) :: rest => fieldAnnotations(rest, JFieldAnnotations(fieldSignature(ann), result.isPure))
    case (CommonNames.pureClassName, _) :: rest => fieldAnnotations(rest, JFieldAnnotations(result.signature, true))
    case (_, ann) :: rest                 => fieldAnnotations(rest, result)
    case Nil => result
  }

  private lazy val attributeReader: RuntimeVisibleAnnotationsAttribute =?> List[(String, BAnnotation)] = annotationReader <=< lift { _.annotations }

  private lazy val annotationReader: List[BAnnotation] =?> List[(String, BAnnotation)] = rep { lift { ann => annotationTypeName(ann.annotationType) -> ann } }

  private lazy val classSignature = for {
    metaParams <- array("metaParameters")(elementAnnotation(CommonNames.metaParamClassName, metaParameter))
    supType    <- optional("superType")(classTypeSignature)
    interfaces <- array("interfaces")(classTypeSignature)
  } yield JClassSignature(metaParams, supType | JTypeSignature.objectTypeSig, interfaces)

  private lazy val methodSignature: BAnnotation =?> JMethodSignature = for {
    metaParams  <- array("metaParameters")(elementAnnotation(CommonNames.metaParamClassName, metaParameter))
    retType     <- required("returnType")(typeSignature)
    parameters  <- array("parameters")(parameterSignature)
    exceptions  <- array("throwsTypes")(typeSignature)
    activates   <- array("activates")(typeSignature)
    deactivates <- array("deactivates")(typeSignature)
    requires    <- array("requires")(typeSignature)
  } yield JMethodSignature(metaParams, parameters, retType, exceptions, activates, deactivates, requires)

  private lazy val fieldSignature: BAnnotation =?> JTypeSignature = required("value")(typeSignature)

  private lazy val dsl: BAnnotation =?> DSLInfo = for {
    priorities  <- array("priorities")(string)
    constraints <- array("constraints")(elementAnnotation(CommonNames.constraintClassName, constraint))
    withDSLs    <- array("with")(descriptor)
  } yield DSLInfo(priorities, constraints, withDSLs)

  private lazy val constraint: BAnnotation =?> List[JPriority] = array("value")(elementAnnotation(CommonNames.priorityClassName, priority))

  private lazy val priority: BAnnotation =?> JPriority = for {
    dsl  <- required("dsl")(classTypeSignature)
    name <- required("name")(string)
  } yield JPriority(dsl, name)

  private lazy val operator: BAnnotation =?> JSyntaxDef = for {
    priority <- required("priority")(elementAnnotation(CommonNames.priorityClassName, priority))
    pattern  <- array("pattern")(elementAnnotation(CommonNames.opElemClassName, operatorElement))
    syntax   <- syntaxDef(priority, pattern)
  } yield syntax

  private def syntaxDef (priority: JPriority, pattern: List[JSyntaxElementDef]): BAnnotation =?> JSyntaxDef = enumSwitch("level", CommonNames.opLevelClassName) {
    case "Statement"  => unit(JStatementSyntaxDef(priority, pattern))
    case "Expression" => unit(JExpressionSyntaxDef(priority, pattern))
    case "Literal"    => unit(JLiteralSyntaxDef(priority, pattern))
    case _            => unit(JExpressionSyntaxDef(priority, pattern))
  }

  private lazy val metaParameter: BAnnotation =?> FormalMetaParameter = for {
    name     <- required("name")(string)
    metaType <- optional("type")(typeSignature)
    bounds   <- array("bounds")(typeSignature)
  } yield FormalMetaParameter(name, metaType | JTypeSignature.typeTypeSig, bounds)

  private lazy val operatorElement: BAnnotation =?> JSyntaxElementDef = enumSwitch("kind", CommonNames.opElemTypeClassName) {
    case "Name"         => required("name")(string).map(JOperatorNameDef)
    case "Regex"        => required("name")(string).map(JRegexNameDef)
    case "Hole"         => priorityOptArray.map(JOperandDef)
    case "Star"         => priorityOptArray.map(JRepetition0Def)
    case "Plus"         => priorityOptArray.map(JRepetition1Def)
    case "Optional"     => priorityOptArray.map(JOptionalOperandDef)
    case "AndPredicate" => for {
      name     <- required("name")(typeSignature)
      priority <- priorityOptArray
    } yield JAndPredicateDef(name, priority)
    case "NotPredicate" => for {
      name     <- required("name")(typeSignature)
      priority <- priorityOptArray
    } yield JNotPredicateDef(name, priority)
    case "Reference"    => for {
      name     <- required("name")(string)
      priority <- priorityOptArray
    } yield JMetaValueRefDef(name, priority)
    case _              => state.errorAndReturn("invalid operator element type", unit(JOperandDef(None)))
  }

  private lazy val priorityOptArray = optArray("priority")(elementAnnotation(CommonNames.priorityClassName, priority))

  private def enumSwitch [T] (name: String, enumTypeName: String)(readers: String => BAnnotation =?> T): BAnnotation =?> T = opt(elem(name)) >=> collect {
    case Some(BAnnotationElement_Enum(e, c)) if annotationTypeName(e) == enumTypeName => readUTF(c)
    case None => ""
  } flatMap readers

  private def required [T] (name: String)(reader: BAnnotationElement =?> T): BAnnotation =?> T = elem(name) >=> reader

  private def optional [T] (name: String)(reader: BAnnotationElement =?> T): BAnnotation =?> Option[T] = opt(elem(name) >=> reader)

  private def optArray [T] (name: String)(reader: BAnnotationElement =?> T): BAnnotation =?> Option[T] = opt(elem(name) >==> {
    case BAnnotationElement_Array(array) => array.headOption
    case elem => Some(elem)
  } >=> reader)

  private def array [T] (name: String)(reader: BAnnotationElement =?> T): BAnnotation =?> List[T] = opt(elem(name)).map {
    case Some(BAnnotationElement_Array(array)) => array
    case elem => elem.toList
  } >=> rep(reader)

  private def elem (name: String): BAnnotation =?> BAnnotationElement = elements >=> getElement(name)

  private def getElement (name: String): Map[String, BAnnotationElement] =?> BAnnotationElement = read(_.get(name))

  private lazy val elements: BAnnotation =?> Map[String, BAnnotationElement] = lift {
    mutableHashMapMemo { _.values.map { case (k, v) => readUTF(k) -> v }.toMap }
  }

  private def elementAnnotation [T] (desc: String, reader: BAnnotation =?> T): BAnnotationElement =?> T = reader <=< collect {
    case BAnnotationElement_Annotation(ann) if annotationTypeName(ann.annotationType) == desc => ann
  }

  private lazy val string: BAnnotationElement =?> String = read {
    case BAnnotationElement_String(str) => Some(readUTF(str))
    case e => state.errorAndReturn("invalid annotation element : expected string, but found " + e, None)
  }

  private lazy val descriptor: BAnnotationElement =?> JClassTypeSignature = read {
    case BAnnotationElement_Class(ref) => DescriptorParsers.parseClassTypeDescriptor(readUTF(ref))
    case e => state.errorAndReturn("invalid annotation element : expected class, but found " + e, None)
  }

  private lazy val typeSignature: BAnnotationElement =?> JTypeSignature = string >==> SignatureParsers.parseTypeSignature

  private lazy val classTypeSignature: BAnnotationElement =?> JClassTypeSignature = string >==> SignatureParsers.parseClassTypeSignature

  private lazy val parameterSignature: BAnnotationElement =?> JParameterSignature = string >==> SignatureParsers.parseParameterSignature

  private def annotationTypeName (annType: Int): String = {
    val name = readUTF(annType)
    if (name.startsWith("L") && name.endsWith(";")) name.tail.init
    else name
  }

  private def unit [A, B](b: => B): A =?> B = Kleisli { _ => Some(b) }
  private def lift [A, B](f: A => B): A =?> B = Kleisli { a => Some(f(a)) }
  private def read [A, B](f: A => Option[B]): A =?> B = Kleisli[Option, A, B](f)
  private def collect [A, B](f: PartialFunction[A, B]): A =?> B = read(f.lift)

  private def rep [A, B] (reader: A =?> B): List[A] =?> List[B] = read(_.traverse(reader(_)))
  private def opt [A, B] (reader: A =?> B): A =?> Option[B] = read { a => Some(reader(a)) }
}
