package phenan.prj.ir

import phenan.prj._
import phenan.prj.declaration._
import phenan.prj.signature._

import CommonNames._

import scalaz._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.std.option.optionSyntax._
import scalaz.syntax.traverse._

trait IRAnnotationReader {
  this: NameResolvers with JTypeLoader with JClassLoader with SignatureParser with IRs with IRExpressions with JModules with JErasedTypes with Application =>

  trait IRAnnotationReaders {

    def file: IRFile

    def readAnnotations (as: List[Annotation]): List[IRAnnotation] = annotations(as).getOrElse(Nil)

    lazy val readClassSignature: List[IRAnnotation] => Option[JClassSignature] = reader(classSigClassName, classSignatureAnnotation)
    lazy val readMethodSignature: List[IRAnnotation] => Option[JMethodSignature] = reader(methodSigClassName, methodSignatureAnnotation)
    lazy val readFieldSignature: List[IRAnnotation] => Option[JTypeSignature] = reader(fieldSigClassName, fieldSignatureAnnotation)
    lazy val readOperator: List[IRAnnotation] => Option[JSyntaxDef] = reader(operatorClassName, operatorAnnotation)
    lazy val readDsl: List[IRAnnotation] => Option[IRDSLInfo] = reader(dslClassName, dslAnnotation)

    /* read IRAnnotation */

    private def reader [T] (name: String, reader: IRAnnotation =?> T): List[IRAnnotation] => Option[T] = { as =>
      reader =<< as.find(_.annotationClass.internalName == name)
    }

    //private def marker (name: String): List[IRAnnotation] => Boolean = { _.exists(_.annotationClass.internalName == name) }

    private lazy val classSignatureAnnotation = for {
      metaParams <- array("metaParameters")(elementAnnotation(metaParamClassName, metaParameterAnnotation))
      supType    <- optional("superType")(classTypeSignature)
      interfaces <- array("interfaces")(classTypeSignature)
    } yield JClassSignature(metaParams, supType | JTypeSignature.objectTypeSig, interfaces)

    private lazy val methodSignatureAnnotation: IRAnnotation =?> JMethodSignature = for {
      metaParams  <- array("metaParameters")(elementAnnotation(metaParamClassName, metaParameterAnnotation))
      retType     <- required("returnType")(typeSignature)
      retBounds   <- array("returnBounds")(typeSignature)
      parameters  <- array("parameters")(parameterSignature)
      exceptions  <- array("throwsTypes")(typeSignature)
      activates   <- array("activates")(typeSignature)
      deactivates <- array("deactivates")(typeSignature)
      requires    <- array("requires")(typeSignature)
    } yield JMethodSignature(metaParams, parameters, retType, retBounds, exceptions, activates, deactivates, requires)

    private lazy val fieldSignatureAnnotation: IRAnnotation =?> JTypeSignature = required("value")(typeSignature)

    private lazy val dslAnnotation: IRAnnotation =?> IRDSLInfo = for {
      priorities  <- array("priorities")(string)
      constraints <- array("constraints")(elementAnnotation(constraintClassName, constraintAnnotation))
      withDSLs    <- array("with")(descriptor)
    } yield IRDSLInfo(priorities, constraints, withDSLs)

    private lazy val constraintAnnotation: IRAnnotation =?> List[JPriority] = array("value")(elementAnnotation(priorityClassName, priorityAnnotation))

    private lazy val priorityAnnotation: IRAnnotation =?> JPriority = for {
      dsl  <- required("dsl")(classTypeSignature)
      name <- required("name")(string)
    } yield JPriority(dsl, name)

    private lazy val operatorAnnotation: IRAnnotation =?> JSyntaxDef = for {
      priority <- required("priority")(elementAnnotation(priorityClassName, priorityAnnotation))
      pattern  <- array("pattern")(elementAnnotation(opElemClassName, operatorElementAnnotation))
      syntax   <- syntaxDefAnnotation(priority, pattern)
    } yield syntax

    private def syntaxDefAnnotation (priority: JPriority, pattern: List[JSyntaxElementDef]): IRAnnotation =?> JSyntaxDef = enumSwitch("level") {
      case "Statement"  => unit(JStatementSyntaxDef(priority, pattern))
      case "Expression" => unit(JExpressionSyntaxDef(priority, pattern))
      case "Literal"    => unit(JLiteralSyntaxDef(priority, pattern))
      case _            => unit(JExpressionSyntaxDef(priority, pattern))
    }

    private lazy val metaParameterAnnotation: IRAnnotation =?> FormalMetaParameter = for {
      name     <- required("name")(string)
      metaType <- optional("type")(typeSignature)
      bounds   <- array("bounds")(typeSignature)
    } yield FormalMetaParameter(name, metaType | JTypeSignature.typeTypeSig, bounds)

    private lazy val operatorElementAnnotation: IRAnnotation =?> JSyntaxElementDef = enumSwitch("kind") {
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

    private lazy val priorityOptArray = optArray("priority")(elementAnnotation(priorityClassName, priorityAnnotation))

    private def enumSwitch [T] (name: String)(readers: String => IRAnnotation =?> T): IRAnnotation =?> T = opt(elem(name)) >=> read {
      case Some(IREnumConstantRef(field)) => Some(field.name)
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

    private lazy val typeSignature: IRAnnotationElement =?> JTypeSignature = string >==> parseTypeSignature
    private lazy val classTypeSignature: IRAnnotationElement =?> JClassTypeSignature = string >==> parseClassTypeSignature
    private lazy val parameterSignature: IRAnnotationElement =?> JParameterSignature = string >==> parseParameterSignature

    private lazy val string: IRAnnotationElement =?> String = collect { case IRStringLiteral(s) => s }
    private lazy val descriptor: IRAnnotationElement =?> JClass = collect { case IRObjectClassLiteral(clazz, 0) => clazz }


    /* Annotation => IRAnnotation */

    private lazy val annotations: List[Annotation] =?> List[IRAnnotation] = rep(annotation)

    private lazy val annotation: Annotation =?> IRAnnotation = for {
      clazz <- annotationClass
      args  <- annotationArguments(clazz)
    } yield IRAnnotation(clazz, args)

    private lazy val annotationClass: Annotation =?> JClass = read { ann => resolver.resolve(ann.name.names).toOption }

    private def annotationArguments (clazz: JClass): Annotation =?> Map[String, IRAnnotationElement] = read {
      case MarkerAnnotation(_)             => Some(Map.empty)
      case SingleElementAnnotation(_, arg) => annotationElement("value", arg, clazz).map(Map(_))
      case FullAnnotation(_, args)         => args.toList.traverse[Option, (String, IRAnnotationElement)] { case (s, e) => annotationElement(s, e, clazz) }.map(_.toMap)
    }

    private def annotationElement (name: String, arg: AnnotationElement, clazz: JClass): Option[(String, IRAnnotationElement)] = {
      clazz.methods.find(_.name == name).flatMap(method => annotationElement(arg, method.erasedReturnType).map(name -> _))
    }

    private def annotationElement (arg: AnnotationElement, annType: JErasedType): Option[IRAnnotationElement] = annType match {
      case arr: JArrayClass                  => annotationElement_Array(arg, arr.component)
      case enm: JClass if enm.isEnum         => annotationElement_EnumConstant(arg, enm)
      case ann: JClass if ann.isAnnotation   => annotationElement_Annotation(arg, ann)
      case prm: JPrimitiveClass              => evaluate(arg, prm.primitiveType)
      case str: JClass if stringClass == str => evaluate(arg, stringType)
    }

    private def annotationElement_Array (arg: AnnotationElement, component: JErasedType): Option[IRAnnotationElementArray] = arg match {
      case ArrayOfAnnotationElement(array) => array.traverse(annotationElement(_, component)).map(IRAnnotationElementArray)
      case _ => None
    }

    private def annotationElement_EnumConstant (arg: AnnotationElement, enum: JClass): Option[IREnumConstantRef] = arg match {
      case EnumConstantElement(name) =>
        if (name.size == 1) enum.fields.find(_.name == name.size).map(IREnumConstantRef)
        else resolver.resolve(name.init).toOption.filter(_ == enum).flatMap(_.fields.find(_.name == name.last)).map(IREnumConstantRef)
      case _ => None
    }

    private def annotationElement_Annotation (arg: AnnotationElement, annotationType: JClass): Option[IRAnnotation] = arg match {
      case ann: Annotation => annotation(ann).filter(_.annotationClass == annotationType)
      case _ => None
    }

    private def evaluate (arg: AnnotationElement, expected: JType): Option[IRAnnotationElement] = arg match {
      case e: AnnotationExpression => evaluate(e).filter(_.staticType <:< expected)
      case _ => None
    }

    private def evaluate (arg: AnnotationExpression): Option[IRJavaLiteral] = arg match {
      case StringLiteralExpression(s)   => Some(IRStringLiteral(s))
      case ClassLiteralExpression(name) =>
        if (name.name.names.size == 1 && primitives.contains(name.name.names.head))
          Some(IRPrimitiveClassLiteral(primitives(name.name.names.head).primitiveType, name.dim))
        else
          resolver.resolve(name.name.names).toOption.map(IRObjectClassLiteral(_, name.dim))
    }

    private def unit [A, B](b: => B): A =?> B = Kleisli { _ => Some(b) }
    private def read [A, B](f: A => Option[B]): A =?> B = Kleisli[Option, A, B](f)

    private def collect [A, B](f: PartialFunction[A, B]): A =?> B = read(f.lift)
    private def rep [A, B] (reader: A =?> B): List[A] =?> List[B] = read(_.traverse(reader(_)))
    private def opt [A, B] (reader: A =?> B): A =?> Option[B] = read { a => Some(reader(a)) }

    private def resolver: NameResolver = file.resolver
  }

}
