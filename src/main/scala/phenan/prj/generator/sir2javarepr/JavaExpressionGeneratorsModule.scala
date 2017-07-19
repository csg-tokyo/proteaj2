package phenan.prj.generator.sir2javarepr

import phenan.prj.JModules
import phenan.prj.generator.SimplifiedIRs
import phenan.prj.generator.JavaRepr._
import phenan.util._

/**
  * Created by ichikawa on 2017/07/14.
  */
trait JavaExpressionGeneratorsModule {
  this: JavaStatementGeneratorsModule with JavaSignatureGeneratorsModule with JavaLiteralsGeneratorsModule with SimplifiedIRs with JModules =>

  object JavaExpressionGenerators {
    import JavaSignatureGenerators._

    def expression (expr: SIRExpression): Expression = expr match {
      case SIRLambda(parameters, body) =>
        Union[Expression](Lambda(parameters.map { case (t, n) => Param(typeToSig(t), n) }, JavaStatementGenerators.block(body)))
      case SIRNewArray(t, size, dim) =>
        Union[Expression](NewArray(typeToSig(t), size.map(expression), dim))
      case SIRArrayInit(t, dim, components) =>
        Union[Expression](ArrayInit(typeToSig(t), dim, components.map(expression)))
      case SIRArrayAccess(array, index) =>
        Union[Expression](ArrayAccess(expression(array), expression(index)))
      case SIRAssignmentExpression(left, right) =>
        Union[Expression](Assignment(expression(left), expression(right)))
      case SIRCastExpression(t, e) =>
        Union[Expression](CastExpression(typeToSig(t), expression(e)))
      case SIRConstructorCall(metaArgs, constructType, args) =>
        Union[Expression](NewExpression(metaArgs.flatMap(metaArgument), objectType(constructType), args.map(expression)))
      case SIRInstanceMethodCall(instance, metaArgs, methodName, args) =>
        Union[Expression](MethodCall(receiver(instance), metaArgs.flatMap(metaArgument), methodName, args.map(expression)))
      case SIRStaticMethodCall(clazz, metaArgs, methodName, args) =>
        Union[Expression](MethodCall(receiver(clazz), metaArgs.flatMap(metaArgument), methodName, args.map(expression)))
      case SIRSuperMethodCall(thisType, metaArgs, methodName, args) =>
        Union[Expression](MethodCall(superRef(thisType), metaArgs.flatMap(metaArgument), methodName, args.map(expression)))
      case SIRInstanceFieldAccess(instance, fieldName) =>
        Union[Expression](FieldAccess(receiver(instance), fieldName))
      case SIRStaticFieldAccess(clazz, fieldName) =>
        Union[Expression](FieldAccess(receiver(clazz), fieldName))
      case SIRSuperFieldAccess(thisType, fieldName) =>
        Union[Expression](FieldAccess(superRef(thisType), fieldName))
      case SIRThisRef(thisType) =>
        Union[Expression](ThisRef(ClassRef(thisType.erase.name)))
      case SIRLocalRef(name) =>
        Union[Expression](LocalRef(name))
      case literal: SIRJavaLiteral =>
        Union[Expression](JavaLiteralsGenerators.javaLiteral(literal))
      case SIRDummyExpression =>
        throw new RuntimeException("broken simplified IR")
    }

    def receiver (instance: SIRExpression): Receiver = Union[Receiver](expression(instance))
    def receiver (module: JClassModule): Receiver = Union[Receiver](ClassRef(module.clazz.name))
    def superRef (thisType: JObjectType): Receiver = Union[Receiver](SuperRef(ClassRef(thisType.erase.name)))
  }
}
