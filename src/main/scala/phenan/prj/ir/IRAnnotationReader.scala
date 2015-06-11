package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._
import phenan.prj.signature._
import phenan.prj.state.JState

import scalaz._
import Scalaz._

class IRAnnotationReader (resolver: NameResolver) {

  private lazy val classSig = annotation("proteaj/lang/ClassSig") {
    for {
      metaParams <- array("metaParameters")(elementAnnotation(metaParameter))
      supType    <- optional("superType")(classTypeSignature)
      interfaces <- array("interfaces")(classTypeSignature)
    } yield JClassSignature(metaParams, supType | JTypeSignature.objectTypeSig, interfaces)
  }

  private lazy val methodSignature: List[Annotation] =?> JMethodSignature = annotation("proteaj/lang/MethodSig") {
    for {
      metaParams  <- array("metaParameters")(elementAnnotation(metaParameter))
      retType     <- required("returnType")(typeSignature)
      parameters  <- array("parameters")(parameterSignature)
      exceptions  <- array("throwsTypes")(typeSignature)
      activates   <- array("activates")(typeSignature)
      deactivates <- array("deactivates")(typeSignature)
      requires    <- array("requires")(typeSignature)
    } yield JMethodSignature(metaParams, parameters, retType, exceptions, activates, deactivates, requires)
  }

  private lazy val fieldSignature: List[Annotation] =?> JTypeSignature = annotation("proteaj/lang/FieldSig") {
    required("value")(typeSignature)
  }

  private lazy val dsl: List[Annotation] =?> DSLInfo = annotation("proteaj/lang/DSL") {
    for {
      priorities <- array("priorities")(string)
      withDSLs   <- array("with")(descriptor)
    } yield DSLInfo(priorities, withDSLs)
  }

  private lazy val operator: List[Annotation] =?> JOperatorSyntaxDef = annotation("proteaj/lang/Operator") {
    optional("priority")(string).flatMap { priority =>
      array("pattern")(elementAnnotation(operatorElement)).flatMap { pattern =>
        enumSwitch("level", "proteaj/lang/OpLevel") {
          case "Statement"  => unit(JStatementSyntaxDef(priority, pattern))
          case "Expression" => unit(JExpressionSyntaxDef(priority, pattern))
          case "Literal"    => unit(JLiteralSyntaxDef(priority, pattern))
          case _            => unit(JExpressionSyntaxDef(priority, pattern))
        }
      }
    }
  }

  private lazy val metaParameter: List[Annotation] =?> FormalMetaParameter = annotation("proteaj/lang/MetaParameter") {
    for {
      name     <- required("name")(string)
      metaType <- optional("type")(typeSignature)
      priority <- optional("priority")(string)
      bounds   <- array("bounds")(typeSignature)
    } yield FormalMetaParameter(name, metaType | JTypeSignature.typeTypeSig, priority, bounds)
  }

  private lazy val operatorElement: List[Annotation] =?> JSyntaxElementDef = annotation("proteaj/lang/OpElem") {
    enumSwitch("kind", "proteaj/lang/OpElemType") {
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
  }

  private def annotation [T] (desc: String)(reader: Annotation =?> T): List[Annotation] =?> T = annotationReader >==> { as =>
    compiler.classLoader.loadClass_PE(desc).flatMap(as.get)
  } >=> reader

  private lazy val annotationReader: List[Annotation] =?> Map[JClass, Annotation] = lift(_.flatMap { a => resolver.resolve(a.name.names).map(_ -> a).toOption }.toMap)


  private def enumSwitch [T] (name: String, enumTypeName: String)(readers: String => Annotation =?> T): Annotation =?> T = elem(name) >=> expression(enumTypeName) >=> collect {
    case ConcretePureValue(en: java.lang.Enum[_], _) => en.name
  } flatMap readers

  private def required [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> T = elem(name) >=> reader

  private def array [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> List[T] = opt(elem(name)).map {
    case Some(ArrayOfAnnotationElement(array)) => array
    case _ => Nil
  } >=> rep(reader)

  private def optional [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> Option[T] = opt(elem(name) >=> reader)


  private def elem (name: String): Annotation =?> AnnotationElement = read {
    case MarkerAnnotation(_)           => None
    case SingleElementAnnotation(_, arg) if name == "value" => Some(arg)
    case SingleElementAnnotation(_, _) => None
    case FullAnnotation(_, args)       => args.get(name)
  }

  private def elementAnnotation [T] (reader: List[Annotation] =?> T): AnnotationElement =?> T = reader <=< collect {
    case ann: Annotation => List(ann)
  }

  private lazy val typeSignature: AnnotationElement =?> JTypeSignature = string >==> SignatureParsers.parseTypeSignature
  private lazy val classTypeSignature: AnnotationElement =?> JClassTypeSignature = string >==> SignatureParsers.parseClassTypeSignature
  private lazy val parameterSignature: AnnotationElement =?> JParameterSignature = string >==> SignatureParsers.parseParameterSignature

  private lazy val string: AnnotationElement =?> String =
    expression(compiler.typeLoader.stringType) >=> collect { case ConcretePureValue(str: java.lang.String, _) => str }

  private lazy val descriptor: AnnotationElement =?> JTypeSignature =
    expression(compiler.typeLoader.anyClassType) >=> collect { case ConcretePureValue(cls: java.lang.Class[_], _) => cls.getName } >==> BinaryNameParsers.parseBinaryName

  private def expression (expected: String): AnnotationElement =?> MetaValue = expression(compiler.classLoader.loadClass_PE(expected).flatMap(_.objectType(Nil)))

  private def expression (expected: => Option[JType]): AnnotationElement =?> MetaValue = read { _: AnnotationElement => expected } flatMap expression

  private def expression (expected: JType): AnnotationElement =?> MetaValue = partial { case ExpressionSnippet(snippet) =>
    compiler.bodyCompiler.expression(snippet, expected, BaseEnvironment(resolver)).flatMap(_.eval).toOption
  }

  private def unit [A, B](b: => B): A =?> B = Kleisli { _ => Some(b) }
  private def lift [A, B](f: A => B): A =?> B = Kleisli { a => Some(f(a)) }
  private def read [A, B](f: A => Option[B]): A =?> B = Kleisli[Option, A, B](f)
  private def collect [A, B](f: PartialFunction[A, B]): A =?> B = read(f.lift)
  private def partial [A, B](f: PartialFunction[A, Option[B]]): A =?> B = read { a => f.applyOrElse(a, { _:A => None }) }

  private def rep [A, B](reader: A =?> B): List[A] =?> List[B] = read(_.traverse(reader(_)))
  private def opt [A, B] (reader: A =?> B): A =?> Option[B] = read { a => Some(reader(a)) }

  def compiler = resolver.root.compiler

  implicit def state: JState = compiler.state
}
