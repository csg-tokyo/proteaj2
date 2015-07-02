package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._
import phenan.prj.signature._
import phenan.prj.state.JState

import scalaz._
import Scalaz._

class IRAnnotationReader (file: IRFile) {

  def read (as: List[Annotation]): List[IRAnnotation] = annotations(as).getOrElse(Nil)

  lazy val classSignature  = reader("proteaj/lang/ClassSig", classSignatureAnnotation)
  lazy val methodSignature = reader("proteaj/lang/MethodSig", methodSignatureAnnotation)
  lazy val fieldSignature  = reader("proteaj/lang/FieldSig", fieldSignatureAnnotation)
  lazy val operator = reader("proteaj/lang/Operator", operatorAnnotation)
  lazy val dsl      = reader("proteaj/lang/DSL", dslAnnotation)
  lazy val context  = marker("proteaj/lang/Context")

  /* read IRAnnotation */

  private def reader [T] (name: String, reader: IRAnnotation =?> T): List[IRAnnotation] => Option[T] = { as =>
    reader =<< as.find(_.annotationClass.internalName == name)
  }

  private def marker (name: String): List[IRAnnotation] => Boolean = { _.exists(_.annotationClass.internalName == name) }

  private lazy val classSignatureAnnotation = for {
    metaParams <- array("metaParameters")(elementAnnotation("proteaj/lang/MetaParameter", metaParameterAnnotation))
    supType    <- optional("superType")(classTypeSignature)
    interfaces <- array("interfaces")(classTypeSignature)
  } yield JClassSignature(metaParams, supType | JTypeSignature.objectTypeSig, interfaces)

  private lazy val methodSignatureAnnotation: IRAnnotation =?> JMethodSignature = for {
    metaParams  <- array("metaParameters")(elementAnnotation("proteaj/lang/MetaParameter", metaParameterAnnotation))
    retType     <- required("returnType")(typeSignature)
    parameters  <- array("parameters")(parameterSignature)
    exceptions  <- array("throwsTypes")(typeSignature)
    activates   <- array("activates")(typeSignature)
    deactivates <- array("deactivates")(typeSignature)
    requires    <- array("requires")(typeSignature)
  } yield JMethodSignature(metaParams, parameters, retType, exceptions, activates, deactivates, requires)

  private lazy val fieldSignatureAnnotation: IRAnnotation =?> JTypeSignature = required("value")(typeSignature)

  private lazy val dslAnnotation: IRAnnotation =?> DSLInfo = for {
    priorities  <- array("priorities")(string)
    constraints <- array("constraints")(elementAnnotation("proteaj/lang/Constraint", constraintAnnotation))
    withDSLs    <- array("with")(descriptor)
  } yield DSLInfo(priorities, constraints, withDSLs)

  private lazy val constraintAnnotation: IRAnnotation =?> List[JPriority] = array("value")(elementAnnotation("proteaj/lang/Priority", priorityAnnotation))

  private lazy val priorityAnnotation: IRAnnotation =?> JPriority = for {
    dsl  <- required("dsl")(classTypeSignature)
    name <- required("name")(string)
  } yield JPriority(dsl, name)

  private lazy val operatorAnnotation: IRAnnotation =?> JSyntaxDef = for {
    priority <- required("priority")(elementAnnotation("proteaj/lang/Priority", priorityAnnotation))
    pattern  <- array("pattern")(elementAnnotation("proteaj/lang/OpElem", operatorElementAnnotation))
    syntax   <- syntaxDefAnnotation(priority, pattern)
  } yield syntax

  private def syntaxDefAnnotation (priority: JPriority, pattern: List[JSyntaxElementDef]): IRAnnotation =?> JSyntaxDef = enumSwitch("level", "proteaj/lang/OpLevel") {
    case "Statement"  => unit(JStatementSyntaxDef(priority, pattern))
    case "Expression" => unit(JExpressionSyntaxDef(priority, pattern))
    case "Literal"    => unit(JLiteralSyntaxDef(priority, pattern))
    case _            => unit(JExpressionSyntaxDef(priority, pattern))
  }

  private lazy val metaParameterAnnotation: IRAnnotation =?> FormalMetaParameter = for {
    name     <- required("name")(string)
    metaType <- optional("type")(typeSignature)
    priority <- optArray("priority")(elementAnnotation("proteaj/lang/Priority", priorityAnnotation))
    bounds   <- array("bounds")(typeSignature)
  } yield FormalMetaParameter(name, metaType | JTypeSignature.typeTypeSig, priority, bounds)

  private lazy val operatorElementAnnotation: IRAnnotation =?> JSyntaxElementDef = enumSwitch("kind", "proteaj/lang/OpElemType") {
    case "Name"         => required("name")(string).map(JOperatorNameDef)
    case "Hole"         => unit(JOperandDef)
    case "Star"         => unit(JRepetition0Def)
    case "Plus"         => unit(JRepetition1Def)
    case "Optional"     => unit(JOptionalOperandDef)
    case "AndPredicate" => required("name")(parameterSignature).map(JAndPredicateDef)
    case "NotPredicate" => required("name")(parameterSignature).map(JNotPredicateDef)
    case "Reference"    => required("name")(string).map(JMetaValueRefDef)
    case _              => state.errorAndReturn("invalid operator element type", unit(JOperandDef))
  }

  private def enumSwitch [T] (name: String, enumTypeName: String)(readers: String => IRAnnotation =?> T): IRAnnotation =?> T = opt(elem(name)) >=> read {
    case Some(IRAnnotationElementEnumConstant(clazz, const)) if clazz.forall(_.internalName == enumTypeName) => Some(const)
    case None => Some("")
    case _    => None
  } flatMap readers

  private def required [T] (name: String)(reader: IRAnnotationElement =?> T): IRAnnotation =?> T = elem(name) >=> reader

  private def array [T] (name: String)(reader: IRAnnotationElement =?> T): IRAnnotation =?> List[T] = opt(elem(name)).map {
    case Some(IRAnnotationElementArray(array)) => array
    case elem => elem.toList
  } >=> rep(reader)

  private def optArray [T] (name: String)(reader: IRAnnotationElement =?> T): IRAnnotation =?> Option[T] = opt(elem(name) >==> {
    case IRAnnotationElementArray(array) => array.headOption
    case elem => Some(elem)
  } >=> reader)

  private def optional [T] (name: String)(reader: IRAnnotationElement =?> T): IRAnnotation =?> Option[T] = opt(elem(name) >=> reader)

  private def elementAnnotation [T] (desc: String, reader: IRAnnotation =?> T): IRAnnotationElement =?> T = reader <=< collect {
    case ann: IRAnnotation if ann.annotationClass.internalName == desc => ann
  }

  private def elem (name: String): IRAnnotation =?> IRAnnotationElement = read(_.args.get(name))

  private lazy val typeSignature: IRAnnotationElement =?> JTypeSignature = string >==> SignatureParsers.parseTypeSignature
  private lazy val classTypeSignature: IRAnnotationElement =?> JClassTypeSignature = string >==> SignatureParsers.parseClassTypeSignature
  private lazy val parameterSignature: IRAnnotationElement =?> JParameterSignature = string >==> SignatureParsers.parseParameterSignature

  private lazy val string: IRAnnotationElement =?> String = collect { case IRAnnotationElementString(s) => s }
  private lazy val descriptor: IRAnnotationElement =?> JClassTypeSignature = collect { case IRAnnotationElementClass(sig: JClassTypeSignature) => sig }


  /* Annotation => IRAnnotation */

  private lazy val annotations: List[Annotation] =?> List[IRAnnotation] = rep(annotation)

  private lazy val annotation: Annotation =?> IRAnnotation = for {
    clazz <- annotationClass
    args  <- annotationArguments
  } yield IRAnnotation(clazz, args)

  private lazy val annotationClass: Annotation =?> JClass = read { ann => resolver.resolve(ann.name.names).toOption }

  private lazy val annotationArguments: Annotation =?> Map[String, IRAnnotationElement] = lift {
    case MarkerAnnotation(_)             => Map.empty
    case SingleElementAnnotation(_, arg) => annotationElement(arg).map(a => Map("value" -> a)).getOrElse(Map.empty)
    case FullAnnotation(_, args)         => args.flatMap { case (s, e) => annotationElement(e).map(s -> _) }
  }

  private lazy val annotationElement: AnnotationElement =?> IRAnnotationElement = read {
    case ann: Annotation                 => annotation(ann)
    case ArrayOfAnnotationElement(array) => rep(annotationElement)(array).map(IRAnnotationElementArray)
    case e: AnnotationExpression         => evaluate(e)
  }

  private lazy val evaluate: AnnotationExpression =?> IRAnnotationElement = read {
    case StringLiteralExpression(s)   => Some(IRAnnotationElementString(s))
    case ClassLiteralExpression(name) => resolver.typeSignature(name).toOption.map(IRAnnotationElementClass)
    case EnumConstantExpression(name) =>
      if (name.size == 1) Some(IRAnnotationElementEnumConstant(None, name.head))
      else resolver.resolve(name.init).toOption.map(clazz => IRAnnotationElementEnumConstant(Some(clazz), name.last))
  }

  private def unit [A, B](b: => B): A =?> B = Kleisli { _ => Some(b) }
  private def lift [A, B](f: A => B): A =?> B = Kleisli { a => Some(f(a)) }
  private def read [A, B](f: A => Option[B]): A =?> B = Kleisli[Option, A, B](f)

  private def collect [A, B](f: PartialFunction[A, B]): A =?> B = read(f.lift)
  private def rep [A, B] (reader: A =?> B): List[A] =?> List[B] = read(_.traverse(reader(_)))
  private def opt [A, B] (reader: A =?> B): A =?> Option[B] = read { a => Some(reader(a)) }

  def compiler = file.compiler
  def resolver = file.resolver

  implicit def state: JState = file.state
}
