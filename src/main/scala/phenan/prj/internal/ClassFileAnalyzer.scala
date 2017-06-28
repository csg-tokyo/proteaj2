package phenan.prj.internal

import phenan.prj._
import phenan.prj.signature._
import phenan.prj.exception._

import scala.reflect.ClassTag

import scalaz._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.option.optionSyntax._
import scalaz.syntax.traverse._
import scalaz.Memo._

/**
  * Created by ichikawa on 2017/06/16.
  */
trait ClassFileAnalyzer {
  this: ClassFileParser with SignatureParser with DescriptorParser with Application =>

  trait AnnotationReader {
    this: ConstantPoolReader =>

    def classAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): JClassAnnotations = classAnnotations(attributeReader =<< attribute | Nil, JClassAnnotations())
    def methodAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): JMethodAnnotations = methodAnnotations(attributeReader =<< attribute | Nil, JMethodAnnotations())
    def fieldAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): JFieldAnnotations = fieldAnnotations(attributeReader =<< attribute | Nil, JFieldAnnotations())

    private def classAnnotations (as: List[(String, BAnnotation)], result: JClassAnnotations): JClassAnnotations = as match {
      case (CommonNames.classSigClassName, ann) :: rest => classAnnotations(rest, result.copy(signature = classSignature(ann)))
      case (CommonNames.dslClassName, ann) :: rest      => classAnnotations(rest, result.copy(dsl = dsl(ann)))
      case (CommonNames.pureClassName, _) :: rest       => classAnnotations(rest, result.copy(isPure = true))
      case (_, _) :: rest                               => classAnnotations(rest, result)
      case Nil => result
    }

    private def methodAnnotations (as: List[(String, BAnnotation)], result: JMethodAnnotations): JMethodAnnotations = as match {
      case (CommonNames.methodSigClassName, ann) :: rest => methodAnnotations(rest, result.copy(signature = methodSignature(ann)))
      case (CommonNames.operatorClassName, ann) :: rest  => methodAnnotations(rest, result.copy(operator = operator(ann)))
      case (CommonNames.pureClassName, _) :: rest        => methodAnnotations(rest, result.copy(isPure = true))
      case (CommonNames.finalizerClassName, _) :: rest   => methodAnnotations(rest, result.copy(isFinalizer = true))
      case (_, _) :: rest                                => methodAnnotations(rest, result)
      case Nil => result
    }

    private def fieldAnnotations (as: List[(String, BAnnotation)], result: JFieldAnnotations): JFieldAnnotations = as match {
      case (CommonNames.fieldSigClassName, ann) :: rest => fieldAnnotations(rest, result.copy(signature = fieldSignature(ann)))
      case (CommonNames.pureClassName, _) :: rest       => fieldAnnotations(rest, result.copy(isPure = true))
      case (_, _) :: rest                               => fieldAnnotations(rest, result)
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
      retType     <- required("returnType")(returnTypeSignature)
      retBounds   <- array("returnBounds")(typeSignature)
      parameters  <- array("parameters")(parameterSignature)
      exceptions  <- array("throwsTypes")(typeSignature)
      activates   <- array("activates")(typeSignature)
      deactivates <- array("deactivates")(typeSignature)
      requires    <- array("requires")(typeSignature)
    } yield JMethodSignature(metaParams, parameters, retType, retBounds, exceptions, activates, deactivates, requires)

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
      case _              => errorAndReturn("invalid operator element type", unit(JOperandDef(None)))
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
      case e => errorAndReturn("invalid annotation element : expected string, but found " + e, None)
    }

    private lazy val descriptor: BAnnotationElement =?> JClassTypeSignature = read {
      case BAnnotationElement_Class(ref) => parseClassTypeDescriptor(readUTF(ref))
      case e => errorAndReturn("invalid annotation element : expected class, but found " + e, None)
    }

    private lazy val typeSignature: BAnnotationElement =?> JTypeSignature = string >==> parseTypeSignature

    private lazy val returnTypeSignature: BAnnotationElement =?> JTypeSignature = string >==> parseReturnTypeSignature

    private lazy val classTypeSignature: BAnnotationElement =?> JClassTypeSignature = string >==> parseClassTypeSignature

    private lazy val parameterSignature: BAnnotationElement =?> JParameterSignature = string >==> parseParameterSignature

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

  trait AttributeParsers {
    this: ConstantPoolReader =>

    def parseClassAttributes (attributes: List[BAttributeInfo]): ClassAttributes = parseClassAttributes(attributes, ClassAttributes(None, None, None))
    def parseFieldAttributes (attributes: List[BAttributeInfo]): FieldAttributes = parseFieldAttributes(attributes, FieldAttributes(None, None))
    def parseMethodAttributes (attributes: List[BAttributeInfo]): MethodAttributes = parseMethodAttributes(attributes, MethodAttributes(None, None, None))

    private def parseClassAttributes (attributes: List[BAttributeInfo], classAttr: ClassAttributes): ClassAttributes = attributes match {
      case attr :: rest => readAs[BUtf8Value](attr.tag).value match {
        case "InnerClasses" => parseClassAttributes(rest, classAttr.copy(innerClasses = parseInnerClassesAttribute(attr)))
        case "Signature" => parseClassAttributes(rest, classAttr.copy(signature = parseSignatureAttribute(attr)))
        case "RuntimeVisibleAnnotations" => parseClassAttributes(rest, classAttr.copy(annotations = parseRuntimeVisibleAnnotationsAttribute(attr)))
        case _ => parseClassAttributes(rest, classAttr)
      }
      case Nil => classAttr
    }

    private def parseFieldAttributes (attributes: List[BAttributeInfo], fieldAttr: FieldAttributes): FieldAttributes = attributes match {
      case attr :: rest => readAs[BUtf8Value](attr.tag).value match {
        case "Signature" => parseFieldAttributes(rest, fieldAttr.copy(signature = parseSignatureAttribute(attr)))
        case "RuntimeVisibleAnnotations" => parseFieldAttributes(rest, fieldAttr.copy(annotations = parseRuntimeVisibleAnnotationsAttribute(attr)))
        case _ => parseFieldAttributes(rest, fieldAttr)
      }
      case Nil => fieldAttr
    }

    private def parseMethodAttributes (attributes: List[BAttributeInfo], methodAttr: MethodAttributes): MethodAttributes = attributes match {
      case attr :: rest => readAs[BUtf8Value](attr.tag).value match {
        case "Signature" => parseMethodAttributes(rest, methodAttr.copy(signature = parseSignatureAttribute(attr)))
        case "Exceptions" => parseMethodAttributes(rest, methodAttr.copy(exceptions = parseExceptionsAttribute(attr)))
        case "RuntimeVisibleAnnotations" => parseMethodAttributes(rest, methodAttr.copy(annotations = parseRuntimeVisibleAnnotationsAttribute(attr)))
        case _ => parseMethodAttributes(rest, methodAttr)
      }
      case Nil => methodAttr
    }
  }

  trait ConstantPoolReader {
    def pool: BConstantPool

    def readAs[T <: BConstant] (index: Int)(implicit Tag: ClassTag[T]): T = pool.constants(index) match {
      case Tag(value) => value
      case _ =>
        error("constant pool is broken")
        throw InvalidClassFileException("constant pool is broken")
    }

    def readAsOption[T <: BConstant] (index: Int)(implicit tag: ClassTag[T]): Option[T] = {
      if (index == 0) None
      else tag.unapply(pool.constants(index))
    }

    def readUTF (ref: Int): String = readAs[BUtf8Value](ref).value

    def readClassName (ref: Int): String = readAs[BUtf8Value](readAs[BClassRef](ref).ref).value
    def readClassNameOption (ref: Int): Option[String] = readAsOption[BClassRef](ref).flatMap(r => readAsOption[BUtf8Value](r.ref).map(_.value))
  }
}
