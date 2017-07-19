package phenan.prj.generator

import phenan.prj._

/**
  * Created by ichikawa on 2017/07/05.
  */
trait SimplifiedIRs {
  this: JModules with JMembers with JErasedTypes with Application =>

  case class SIRFile (packageName: Option[String], modules: List[SIRModule])

  sealed trait SIRModule extends SIRMember {
    def annotations: List[SIRAnnotation]
    def signature: JClassSignature

    def metaParams: List[FormalMetaParameter] = signature.metaParams
    def superClass: JClassTypeSignature       = signature.superClass
    def interfaces: List[JClassTypeSignature] = signature.interfaces
  }

  case class SIRClass
  (annotations: List[SIRAnnotation],
   modifiers: JModifier,
   name: String,
   signature: JClassSignature,
   declaredPriorities: Set[JPriority],
   priorityConstraints: List[List[JPriority]],
   withDSLs: List[JClass],
   members: List[SIRMember]
  ) extends SIRModule

  case class SIRInterface
  (annotations: List[SIRAnnotation],
   modifiers: JModifier,
   name: String,
   signature: JClassSignature,
   members: List[SIRMember]
  ) extends SIRModule

  case class SIREnum
  (annotations: List[SIRAnnotation],
   modifiers: JModifier,
   name: String,
   signature: JClassSignature,
   constants: List[SIREnumConstant],
   members: List[SIRMember]
  ) extends SIRModule

  sealed trait SIRMember

  sealed trait SIRProcedure extends SIRMember {
    def annotations: List[SIRAnnotation]
    def signature: JMethodSignature
    def parameterNames: List[String]
    def contextNames: List[String]

    def javaParameters: List[SIRFormalParameter] = {
      val proteajParams = signature.parameters.zip(parameterNames).map {
        case (sig, paramName) => SIRFormalParameter(paramName, sig.actualTypeSignature)
      }
      val contextParams = signature.requires.zip(contextNames).map {
        case (sig, contextName) => SIRFormalParameter(contextName, sig)
      }
      proteajParams ++ contextParams
    }

    def metaParams: List[FormalMetaParameter] = signature.metaParams
    def throwTypes: List[JClassTypeSignature] = signature.throwTypes
  }

  case class SIRMethod
  (annotations: List[SIRAnnotation],
   modifiers: JModifier,
   name: String,
   syntax: Option[JSyntaxDef],
   signature: JMethodSignature,
   parameterNames: List[String],
   contextNames: List[String],
   body: Option[SIRBlock]
  ) extends SIRProcedure
  {
    /* activate x ==> return x
     * return x & activate y & activate z ==> return (x, (y, z))
     */
    def javaReturnType: JTypeSignature = {
      if (signature.activates.isEmpty) signature.returnType
      else if (signature.returnType == VoidTypeSignature) mkReturnTypeList(signature.activates.head, signature.activates.tail)
      else mkReturnTypeList(signature.returnType, signature.activates)
    }

    private def mkReturnTypeList (returnType: JTypeSignature, types: List[JTypeSignature]): JTypeSignature = types match {
      case head :: tail => JTypeSignature.pairTypeSig(returnType, mkReturnTypeList(head, tail))
      case Nil => returnType
    }
  }

  case class SIRConstructor
  (annotations: List[SIRAnnotation],
   modifiers: JModifier,
   className: String,
   signature: JMethodSignature,
   parameterNames: List[String],
   contextNames: List[String],
   body: SIRBlock
  ) extends SIRProcedure

  case class SIRInstanceInitializer (body: SIRBlock) extends SIRMember

  case class SIRStaticInitializer (body: SIRBlock) extends SIRMember

  case class SIRField
  (annotations: List[SIRAnnotation],
   modifiers: JModifier,
   name: String,
   signature: JTypeSignature,
   initializer: Option[SIRExpression]) extends SIRMember

  case class SIRFormalParameter (name: String, paramType: JTypeSignature)

  case class SIREnumConstant (name: String)

  sealed trait SIRStatement

  case class SIRBlock (statements: List[SIRStatement]) extends SIRStatement
  case class SIRLocalDeclaration (localType: JType, name: String, init: Option[SIRExpression]) extends SIRStatement
  case class SIRIfStatement (condition: SIRSimpleExpression, thenStatement: SIRBlock, elseStatement: Option[SIRBlock]) extends SIRStatement
  case class SIREnhancedForStatement(elementType: JType, name: String, iterable: SIRSimpleExpression, body: SIRBlock) extends SIRStatement
  case class SIRWhileStatement (condition: SIRSimpleExpression, body: SIRBlock) extends SIRStatement
  case class SIRTryStatement (tryBlock: SIRBlock, catchBlocks: List[SIRExceptionHandler], finallyBlock: Option[SIRBlock]) extends SIRStatement
  case class SIRThrowStatement (exception: SIRSimpleExpression) extends SIRStatement
  case class SIRReturnStatement (value: Option[SIRSimpleExpression]) extends SIRStatement
  case class SIRExpressionStatement (expression: SIRExpression) extends SIRStatement
  case object SIRBreakStatement extends SIRStatement

  case class SIRThisConstructorCall (metaArguments: List[MetaArgument], arguments: List[SIRExpression]) extends SIRStatement
  case class SIRSuperConstructorCall (metaArguments: List[MetaArgument], arguments: List[SIRExpression]) extends SIRStatement

  case class SIRExceptionHandler (exceptionType: JType, name: String, catchBlock: SIRBlock)

  sealed trait SIRExpression
  sealed trait SIRSimpleExpression extends SIRExpression

  case class SIRLambda (parameters: List[(JType, String)], body: SIRBlock) extends SIRSimpleExpression

  case class SIRNewArray (componentType: JType, arraySize: List[SIRSimpleExpression], dim: Int) extends SIRExpression
  case class SIRArrayInit (componentType: JType, dim: Int, components: List[SIRSimpleExpression]) extends SIRExpression
  case class SIRArrayAccess(array: SIRSimpleExpression, index: SIRSimpleExpression) extends SIRSimpleExpression

  case class SIRAssignmentExpression (left: SIRSimpleExpression, right: SIRSimpleExpression) extends SIRExpression
  case class SIRCastExpression(destType: JType, expression: SIRSimpleExpression) extends SIRExpression

  case class SIRConstructorCall (metaArguments: List[MetaArgument], constructType: JObjectType, arguments: List[SIRSimpleExpression]) extends SIRExpression
  case class SIRInstanceMethodCall (instance: SIRSimpleExpression, metaArguments: List[MetaArgument], methodName: String, arguments: List[SIRSimpleExpression]) extends SIRExpression
  case class SIRStaticMethodCall (classModule: JClassModule, metaArguments: List[MetaArgument], methodName: String, arguments: List[SIRSimpleExpression]) extends SIRExpression
  case class SIRSuperMethodCall (thisType: JObjectType, metaArguments: List[MetaArgument], methodName: String, arguments: List[SIRSimpleExpression]) extends SIRExpression

  case class SIRInstanceFieldAccess (instance: SIRSimpleExpression, fieldName: String) extends SIRSimpleExpression
  case class SIRStaticFieldAccess (classModule: JClassModule, fieldName: String) extends SIRSimpleExpression
  case class SIRSuperFieldAccess (thisType: JObjectType, fieldName: String) extends SIRSimpleExpression

  case class SIRThisRef (thisType: JObjectType) extends SIRSimpleExpression
  case class SIRLocalRef (name: String) extends SIRSimpleExpression
  case object SIRDummyExpression extends SIRSimpleExpression

  sealed trait SIRAnnotationElement

  case class SIRAnnotation (annotationClass: JClass, arguments: Map[String, SIRAnnotationElement]) extends SIRAnnotationElement
  case class SIRAnnotationElementArray (components: List[SIRAnnotationElement]) extends SIRAnnotationElement
  case class SIREnumConstantRef (field: JFieldDef) extends SIRAnnotationElement

  sealed trait SIRJavaLiteral extends SIRSimpleExpression with SIRAnnotationElement

  case class SIRObjectClassLiteral (clazz: JClass, dim: Int) extends SIRJavaLiteral
  case class SIRPrimitiveClassLiteral (primitiveType: JPrimitiveType, dim: Int) extends SIRJavaLiteral
  case class SIRCharLiteral (value: Char) extends SIRJavaLiteral
  case class SIRIntLiteral (value: Int) extends SIRJavaLiteral
  case class SIRLongLiteral (value: Long) extends SIRJavaLiteral
  case class SIRBooleanLiteral (value: Boolean) extends SIRJavaLiteral
  case class SIRStringLiteral (value: String) extends SIRJavaLiteral
  case object SIRNullLiteral extends SIRJavaLiteral
}
