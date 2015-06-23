package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._
import phenan.prj.signature._
import phenan.prj.state.JState

import scalaz._
import Scalaz._

class IRAnnotationReader (file: IRFile) {

  def classAnnotations (as: List[Annotation]): JClassAnnotations = annotationReader(as).map(classAnnotations(_, defaultClassAnnotations)).getOrElse(defaultClassAnnotations)
  def methodAnnotations (as: List[Annotation]): JMethodAnnotations = annotationReader(as).map(methodAnnotations(_, defaultMethodAnnotations)).getOrElse(defaultMethodAnnotations)
  def fieldAnnotations (as: List[Annotation]): JFieldAnnotations = annotationReader(as).map(fieldAnnotations(_, defaultFieldAnnotations)).getOrElse(defaultFieldAnnotations)

  private lazy val defaultClassAnnotations = JClassAnnotations(None, None, false, false, Nil)
  private lazy val defaultMethodAnnotations = JMethodAnnotations(None, None, false, false, Nil)
  private lazy val defaultFieldAnnotations = JFieldAnnotations(None, false, Nil)

  private def classAnnotations (as: List[(String, Annotation)], result: JClassAnnotations): JClassAnnotations = as match {
    case ("proteaj/lang/ClassSig", ann) :: rest => classAnnotations(rest, JClassAnnotations(classSignature(ann), result.dsl, result.isPure, result.isContext, result.others))
    case ("proteaj/lang/DSL", ann) :: rest      => classAnnotations(rest, JClassAnnotations(result.signature, dsl(ann), result.isPure, result.isContext, result.others))
    case ("proteaj/lang/Pure", _) :: rest       => classAnnotations(rest, JClassAnnotations(result.signature, result.dsl, true, result.isContext, result.others))
    case ("proteaj/lang/Context", _) :: rest    => classAnnotations(rest, JClassAnnotations(result.signature, result.dsl, result.isPure, true, result.others))
    case (_, ann) :: rest                       => classAnnotations(rest, JClassAnnotations(result.signature, result.dsl, result.isPure, result.isContext, result.others ++ otherAnnotation(ann)))
    case Nil => result
  }

  private def methodAnnotations (as: List[(String, Annotation)], result: JMethodAnnotations): JMethodAnnotations = as match {
    case ("proteaj/lang/MethodSig", ann) :: rest => methodAnnotations(rest, JMethodAnnotations(methodSignature(ann), result.operator, result.isPure, result.isFinalizer, result.others))
    case ("proteaj/lang/Operator", ann) :: rest  => methodAnnotations(rest, JMethodAnnotations(result.signature, operator(ann), result.isPure, result.isFinalizer, result.others))
    case ("proteaj/lang/Pure", _) :: rest        => methodAnnotations(rest, JMethodAnnotations(result.signature, result.operator, true, result.isFinalizer, result.others))
    case ("proteaj/lang/Finalizer", _) :: rest   => methodAnnotations(rest, JMethodAnnotations(result.signature, result.operator, result.isPure, true, result.others))
    case (_, ann) :: rest                        => methodAnnotations(rest, JMethodAnnotations(result.signature, result.operator, result.isPure, result.isFinalizer, result.others ++ otherAnnotation(ann)))
    case Nil => result
  }

  private def fieldAnnotations (as: List[(String, Annotation)], result: JFieldAnnotations): JFieldAnnotations = as match {
    case ("proteaj/lang/FieldSig", ann) :: rest => fieldAnnotations(rest, JFieldAnnotations(fieldSignature(ann), result.isPure, result.others))
    case ("proteaj/lang/Pure", _) :: rest       => fieldAnnotations(rest, JFieldAnnotations(result.signature, true, result.others))
    case (_, ann) :: rest                       => fieldAnnotations(rest, JFieldAnnotations(result.signature, result.isPure, result.others ++ otherAnnotation(ann)))
    case Nil => result
  }

  private lazy val annotationReader: List[Annotation] =?> List[(String, Annotation)] = rep(annotationClass.flatMap(clazz => lift(clazz.internalName -> _)))

  private lazy val classSignature = for {
    metaParams <- array("metaParameters")(elementAnnotation("proteaj/lang/MetaParameter", metaParameter))
    supType    <- optional("superType")(classTypeSignature)
    interfaces <- array("interfaces")(classTypeSignature)
  } yield JClassSignature(metaParams, supType | JTypeSignature.objectTypeSig, interfaces)

  private lazy val methodSignature: Annotation =?> JMethodSignature = for {
    metaParams  <- array("metaParameters")(elementAnnotation("proteaj/lang/MetaParameter", metaParameter))
    retType     <- required("returnType")(typeSignature)
    parameters  <- array("parameters")(parameterSignature)
    exceptions  <- array("throwsTypes")(typeSignature)
    activates   <- array("activates")(typeSignature)
    deactivates <- array("deactivates")(typeSignature)
    requires    <- array("requires")(typeSignature)
  } yield JMethodSignature(metaParams, parameters, retType, exceptions, activates, deactivates, requires)

  private lazy val fieldSignature: Annotation =?> JTypeSignature = required("value")(typeSignature)

  private lazy val dsl: Annotation =?> DSLInfo = for {
    priorities  <- array("priorities")(string)
    constraints <- array("constraints")(elementAnnotation("proteaj/lang/Constraint", constraint))
    withDSLs    <- array("with")(descriptor)
  } yield DSLInfo(priorities, constraints, withDSLs)

  private lazy val constraint: Annotation =?> List[JPriority] = array("value")(elementAnnotation("proteaj/lang/Priority", priority))

  private lazy val priority: Annotation =?> JPriority = for {
    dsl  <- required("dsl")(classTypeSignature)
    name <- required("name")(string)
  } yield JPriority(dsl, name)

  private lazy val operator: Annotation =?> JSyntaxDef = for {
    priority <- required("priority")(elementAnnotation("proteaj/lang/Priority", priority))
    pattern  <- array("pattern")(elementAnnotation("proteaj/lang/OpElem", operatorElement))
    syntax   <- syntaxDef(priority, pattern)
  } yield syntax

  private def syntaxDef (priority: JPriority, pattern: List[JSyntaxElementDef]): Annotation =?> JSyntaxDef = enumSwitch("level", "proteaj/lang/OpLevel") {
    case "Statement"  => unit(JStatementSyntaxDef(priority, pattern))
    case "Expression" => unit(JExpressionSyntaxDef(priority, pattern))
    case "Literal"    => unit(JLiteralSyntaxDef(priority, pattern))
    case _            => unit(JExpressionSyntaxDef(priority, pattern))
  }

  private lazy val metaParameter: Annotation =?> FormalMetaParameter = for {
    name     <- required("name")(string)
    metaType <- optional("type")(typeSignature)
    priority <- optArray("priority")(elementAnnotation("proteaj/lang/Priority", priority))
    bounds   <- array("bounds")(typeSignature)
  } yield FormalMetaParameter(name, metaType | JTypeSignature.typeTypeSig, priority, bounds)

  private lazy val operatorElement: Annotation =?> JSyntaxElementDef = enumSwitch("kind", "proteaj/lang/OpElemType") {
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

  private lazy val otherAnnotation: Annotation =?> IRUnknownAnnotation = for {
    typ  <- annotationClass >==> { _.objectType(Nil) }
    args <- annotationArguments(typ)
  } yield IRUnknownAnnotation(typ, args)

  private lazy val annotationClass: Annotation =?> JClass = read { ann => resolver.resolve(ann.name.names).toOption }

  private def annotationArguments (annotationType: JObjectType): Annotation =?> Map[JMethod, MetaValue] = {
    allAnnotationElements >=> rep(annotationElement(annotationType))
  }.map(_.toMap)

  private lazy val allAnnotationElements: Annotation =?> List[(String, AnnotationElement)] = lift {
    case MarkerAnnotation(_)             => Nil
    case SingleElementAnnotation(_, arg) => List("value" -> arg)
    case FullAnnotation(_, args)         => args.toList
  }

  private def annotationElement (annotationType: JObjectType): (String, AnnotationElement) =?> (JMethod, MetaValue) = for {
    method <- resolveAnnotationName(annotationType) <=< lift { pair: (String, AnnotationElement) => pair._1 }
    value  <- expression(method.returnType.bind(Map.empty)) <=< lift { pair: (String, AnnotationElement) => pair._2 }
  } yield method -> value

  private def resolveAnnotationName (annotationType: JObjectType): String =?> JMethod = read { name =>
    annotationType.methods.get(name).flatMap(_.headOption)
  }

  private def enumSwitch [T] (name: String, enumTypeName: String)(readers: String => Annotation =?> T): Annotation =?> T = opt(elem(name) >=> expression(enumTypeName)) >=> collect {
    case Some(ConcretePureValue(en: java.lang.Enum[_], _)) => en.name
    case None => ""
  } flatMap readers

  private def required [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> T = elem(name) >=> reader

  private def array [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> List[T] = opt(elem(name)).map {
    case Some(ArrayOfAnnotationElement(array)) => array
    case elem => elem.toList
  } >=> rep(reader)

  private def optArray [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> Option[T] = opt(elem(name) >==> {
    case ArrayOfAnnotationElement(array) => array.headOption
    case elem => Some(elem)
  } >=> reader)

  private def optional [T] (name: String)(reader: AnnotationElement =?> T): Annotation =?> Option[T] = opt(elem(name) >=> reader)


  private def elem (name: String): Annotation =?> AnnotationElement = read {
    case MarkerAnnotation(_)           => None
    case SingleElementAnnotation(_, arg) if name == "value" => Some(arg)
    case SingleElementAnnotation(_, _) => None
    case FullAnnotation(_, args)       => args.get(name)
  }

  private def elementAnnotation [T] (desc: String, reader: Annotation =?> T): AnnotationElement =?> T = reader <=< collect {
    case ann: Annotation if resolver.resolve(ann.name.names).toOption.exists(_.internalName == desc) => ann
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
    compiler.bodyCompiler.expression(snippet, expected, BaseEnvironment(file)).flatMap(_.eval).toOption
  }

  private def unit [A, B](b: => B): A =?> B = Kleisli { _ => Some(b) }
  private def lift [A, B](f: A => B): A =?> B = Kleisli { a => Some(f(a)) }
  private def read [A, B](f: A => Option[B]): A =?> B = Kleisli[Option, A, B](f)
  private def collect [A, B](f: PartialFunction[A, B]): A =?> B = read(f.lift)
  private def partial [A, B](f: PartialFunction[A, Option[B]]): A =?> B = read { a => f.applyOrElse(a, { _:A => None }) }

  private def rep [A, B] (reader: A =?> B): List[A] =?> List[B] = read(_.traverse(reader(_)))
  private def opt [A, B] (reader: A =?> B): A =?> Option[B] = read { a => Some(reader(a)) }

  def compiler = file.compiler
  def resolver = file.resolver

  implicit def state: JState = file.state
}
