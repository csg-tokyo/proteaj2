package phenan.prj.internal

import phenan.prj._
import phenan.prj.signature._
import phenan.prj.state.JState

import scalaz._

class AnnotationReader (classFile: BClassFile)(implicit state: JState) {
  import classFile.poolReader._

  def readClassAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): PrjClassAnnotations =
    attribute.map(classAnnotations).getOrElse(PrjClassAnnotations(None, None, false, false))
  def readMethodAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): PrjMethodAnnotations =
    attribute.map(methodAnnotations).getOrElse(PrjMethodAnnotations(None, None, false, false))
  def readFieldAnnotations (attribute: Option[RuntimeVisibleAnnotationsAttribute]): PrjFieldAnnotations =
    attribute.map(fieldAnnotations).getOrElse(PrjFieldAnnotations(None, false))

  private lazy val classAnnotations = annotations >=> {
    for {
      s <- classSig
      d <- dsl
      p <- pure
      c <- context
    } yield PrjClassAnnotations(s, d, p, c)
  }

  private lazy val methodAnnotations = annotations >=> {
    for {
      sig <- methodSig
      op  <- operator
      pr  <- pure
      fnl <- finalizer
    } yield PrjMethodAnnotations(sig, op, pr, fnl)
  }

  private lazy val fieldAnnotations = annotations >=> {
    for {
      sig <- fieldSig
      pr  <- pure
    } yield PrjFieldAnnotations(sig, pr)
  }

  private lazy val classSig = annotation("ClassSig") {
    for {
      metaParams <- array("metaParameters")(metaParameter)
      supType    <- element("superType")(classTypeSignature)(JTypeSignature.objectTypeSig)
      interfaces <- array("interfaces")(classTypeSignature)
    } yield JClassSignature(metaParams, supType, interfaces)
  }

  private lazy val methodSig = annotation("MethodSig") {
    for {
      metaParams  <- array("metaParameters")(metaParameter)
      retType     <- required("returnType")(typeSignature)(VoidTypeSignature)
      parameters  <- array("parameters")(parameterSignature)
      exceptions  <- array("throwsTypes")(typeSignature)
      activates   <- array("activates")(typeSignature)
      deactivates <- array("deactivates")(typeSignature)
      requires    <- array("requires")(typeSignature)
    } yield JMethodSignature(metaParams, parameters, retType, exceptions, activates, deactivates, requires)
  }

  private lazy val fieldSig = annotation("FieldSig") {
    required("value")(typeSignature)(JTypeSignature.objectTypeSig)
  }

  private lazy val dsl = annotation("DSL") {
    for {
      priorities <- array("priorities")(string)
      withDSLs   <- array("with")(descriptor)
    } yield DSLInfo(priorities, withDSLs)
  }

  private lazy val operator = annotation ("Operator") {
    enumSwitch ("level", "OpLevel") {
      case "Statement"  => statementOperator
      case "Literal"    => literalOperator
      case "Expression" => expressionOperator
    } (expressionOperator)
  }

  private def operatorAnnotation (f: (Option[String], List[JSyntaxElementDef]) => JOperatorSyntaxDef): Reader[Map[String, BAnnotationElement], JOperatorSyntaxDef] = for {
    priority <- optional("priority")(string)
    pat <- pattern
  } yield f(priority, pat)

  private lazy val statementOperator = operatorAnnotation(JStatementSyntaxDef)

  private lazy val expressionOperator = operatorAnnotation(JExpressionSyntaxDef)

  private lazy val literalOperator = operatorAnnotation(JLiteralSyntaxDef)

  private lazy val pure = marker("Pure")

  private lazy val context = marker("Context")

  private lazy val finalizer = marker("Finalizer")

  private lazy val metaParameter = elementAnnotation("MetaParameter") {
    for {
      name      <- required("name")(string)("")
      paramType <- element("type")(typeSignature)(JTypeSignature.typeTypeSig)
      priority  <- optional("priority")(string)
      bounds    <- array("bounds")(typeSignature)
    } yield FormalMetaParameter(name, paramType, priority, bounds)
  }

  private lazy val pattern = array("pattern")(elementAnnotation("OpElem")(operatorElement))

  private lazy val operatorElement = enumSwitch [JSyntaxElementDef] ("kind", "OpElemType") {
    case "Name" => required("name")(string)("") >==> JOperatorNameDef
    case "Hole" => elementReader >==> { _ => JOperandDef }
    case "Star" => elementReader >==> { _ => JRepetition0Def }
    case "Plus" => elementReader >==> { _ => JRepetition1Def }
    case "Optional" => elementReader >==> { _ => JOptionalOperandDef }
    case "AndPredicate" => required("name")(parameterSignature)(defaultPredicateArg) >==> JAndPredicateDef
    case "NotPredicate" => required("name")(parameterSignature)(defaultPredicateArg) >==> JNotPredicateDef
    case "Reference" => required("name")(string)("") >==> JMetaValueRefDef
  } {
    state.error("invalid operator element type")
    elementReader >==> { _ => JOperandDef }
  }

  private lazy val defaultPredicateArg = JParameterSignature(Nil, VoidTypeSignature, None, false, None)

  private lazy val annotations: Reader[RuntimeVisibleAnnotationsAttribute, Map[String, Map[String, BAnnotationElement]]] = Reader {
    _.annotations.map(ann => readUTF(ann.annotationType) -> ann.values.map { case (k, v) => readUTF(k) -> v }.toMap).toMap
  }

  private lazy val annotationReader = Reader(identity[Map[String, Map[String, BAnnotationElement]]])

  private def marker (name: String): Reader[Map[String, Map[String, BAnnotationElement]], Boolean] = {
    annotationReader.map(_.get("Lproteaj/lang/" + name + ";")) >==> { _.nonEmpty }
  }

  private def annotation [T] (name: String)(reader: Reader[Map[String, BAnnotationElement], T]): Reader[Map[String, Map[String, BAnnotationElement]], Option[T]] = {
    annotationReader.map(_.get("Lproteaj/lang/" + name + ";")) >==> { _.map(reader =<< _) }
  }

  private lazy val elementReader = Reader(identity[Map[String, BAnnotationElement]])

  private def required [T] (name: String)(reader: Reader[BAnnotationElement, Option[T]])(default: => T) = element(name)(reader) {
    state.errorAndReturn("required annotation element " + name + " is not found", default)
  }

  private def optional [T] (name: String)(reader: Reader[BAnnotationElement, Option[T]]): Reader[Map[String, BAnnotationElement], Option[T]] = {
    elementReader.map(_.get(name)) >==> { _.flatMap(reader =<< _) }
  }

  private def element [T] (name: String)(reader: Reader[BAnnotationElement, Option[T]])(default: => T): Reader[Map[String, BAnnotationElement], T] = {
    elementReader.map(_.get(name)) >==> { _.flatMap(reader =<< _).getOrElse(default) }
  }

  private def enumSwitch [T] (name: String, enumType: String)(readers: PartialFunction[String, Reader[Map[String, BAnnotationElement], T]])(default: => Reader[Map[String, BAnnotationElement], T]): Reader[Map[String, BAnnotationElement], T] = {
    elementReader.map(_.get(name)).flatMap {
      case Some(BAnnotationElement_Enum(e, c)) if readUTF(e) == "Lproteaj/lang/" + enumType + ";" => readers.applyOrElse(readUTF(c), { _: String => default })
      case _ => default
    }
  }

  private def array [T] (name: String)(reader: Reader[BAnnotationElement, Option[T]]): Reader[Map[String, BAnnotationElement], List[T]] = {
    elementReader.map(_.get(name)) >==> { _.map(array(reader) =<< _).getOrElse(Nil) }
  }

  private def array [T] (reader: Reader[BAnnotationElement, Option[T]]): Reader[BAnnotationElement, List[T]] = Reader {
    case BAnnotationElement_Array(array) => array.flatMap(reader =<< _)
    case e =>
      state.error("invalid annotation element : expected array, but found " + e)
      Nil
  }

  private def elementAnnotation [T] (name: String)(reader: Reader[Map[String, BAnnotationElement], T]): Reader[BAnnotationElement, Option[T]] = Reader {
    case BAnnotationElement_Annotation(ann) if readUTF(ann.annotationType) == "Lproteaj/lang/" + name + ";" =>
      Some(reader =<< ann.values.map { case (k, v) => readUTF(k) -> v }.toMap)
    case e =>
      state.error("invalid annotation element : expected annotation, but found " + e)
      None
  }

  private lazy val string: Reader[BAnnotationElement, Option[String]] = Reader {
    case BAnnotationElement_String(str) => Some(readUTF(str))
    case e =>
      state.error("invalid annotation element : expected string, but found " + e)
      None
  }

  private lazy val descriptor: Reader[BAnnotationElement, Option[JTypeSignature]] = Reader {
    case BAnnotationElement_Class(ref) =>
      DescriptorParsers.parseReturnDescriptor(readUTF(ref))
    case e =>
      state.error("invalid annotation element : expected class, but found " + e)
      None
  }

  private lazy val typeSignature: Reader[BAnnotationElement, Option[JTypeSignature]] = string.map(_.flatMap(SignatureParsers.parseTypeSignature))

  private lazy val classTypeSignature: Reader[BAnnotationElement, Option[JClassTypeSignature]] = string.map(_.flatMap(SignatureParsers.parseClassTypeSignature))

  private lazy val parameterSignature: Reader[BAnnotationElement, Option[JParameterSignature]] = string.map(_.flatMap(SignatureParsers.parseParameterSignature))
}
