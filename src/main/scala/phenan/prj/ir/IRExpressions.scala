package phenan.prj.ir

import phenan.prj._
import phenan.prj.body.EnvModifyStrategy

import scalaz.syntax.traverse._
import scalaz.std.list._
import scalaz.std.option._

trait IRExpressions {
  this: JTypeLoader with Environments with EnvModifyStrategy
    with IRs with IRStatements with JModules with JMembers with JErasedTypes with Application =>

  sealed trait IRExpression {
    def staticType: Option[JType]

    def modifyEnv (env: Environment): Environment

    def withContexts (contexts: List[IRContextRef]): IRExpression = {
      if (contexts.isEmpty) this
      else IRContextualArgument(this, contexts)
    }
  }

  type ParsedArgument     = (Map[String, MetaArgument], IRExpression)
  type ParsedArgumentList = (Map[String, MetaArgument], List[IRExpression])

  case class IRContextualArgument(argument: IRExpression, contexts: List[IRContextRef]) extends IRExpression {
    def staticType: Option[JType] = argument.staticType
    def modifyEnv (env: Environment): Environment = argument.modifyEnv(env)
  }

  case class IRScopeArgument (argument: IRExpression, scopes: List[IRContextRef]) extends IRExpression {
    def staticType: Option[JType] = argument.staticType
    def modifyEnv (env: Environment): Environment = argument.modifyEnv(env).deactivates(scopes)
  }

  sealed trait IRLeftHandSide extends IRExpression

  sealed trait IRAssignmentExpression extends IRExpression {
    def left: IRLeftHandSide
    def right: IRExpression

    def staticType: Option[JType] = right.staticType

    // correct ?
    def modifyEnv (env: Environment): Environment = right.modifyEnv(left.modifyEnv(env))
  }

  case class IRSimpleAssignmentExpression(left: IRLeftHandSide, right: IRExpression) extends IRAssignmentExpression

  case class IRAnonymousClass(metaArgs: Map[String, MetaArgument], baseType: JObjectType, args: List[IRExpression], requiredContexts: List[IRContextRef], members: List[IRClassMember]) extends IRExpression {
    // TODO: staticType should be a sub-type of baseType
    def staticType: Option[JType] = Some(baseType)

    def modifyEnv(env: Environment): Environment = env.modify(args)
  }

  sealed trait IRExplicitConstructorCall

  case class IRThisConstructorCall(metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExplicitConstructorCall

  case class IRSuperConstructorCall(metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExplicitConstructorCall

  sealed trait IRArrayCreation extends IRExpression

  case class IRNewArray(componentType: JType, length: List[IRExpression], dim: Int) extends IRArrayCreation {
    def staticType = Some(componentType.array(length.size + dim))
    def modifyEnv(env: Environment): Environment = env.modify(length)
  }

  case class IRArrayInitializer(componentType: JType, dim: Int, components: List[IRExpression]) extends IRArrayCreation {
    def staticType = Some(componentType.array(dim))
    def modifyEnv(env: Environment): Environment = env.modify(components)
  }

  case class IRCastExpression(destType: JType, expression: IRExpression) extends IRExpression {
    def staticType = Some(destType)

    def modifyEnv(env: Environment): Environment = expression.modifyEnv(env)
  }

  case class IRArrayAccess(array: IRExpression, index: IRExpression) extends IRLeftHandSide {
    def staticType: Option[JType] = array.staticType match {
      case Some(JArrayType(component)) => Some(component)
      case _ => None
    }

    def modifyEnv(env: Environment): Environment = index.modifyEnv(array.modifyEnv(env))
  }

  sealed trait IRFieldAccess extends IRLeftHandSide {
    def field: JField

    def staticType = Some(field.fieldType)
  }

  case class IRInstanceFieldAccess(instance: IRExpression, field: JField) extends IRFieldAccess {
    override def toString: String = instance.toString + '.' + field.name

    def modifyEnv (env: Environment): Environment = instance.modifyEnv(env)
  }

  case class IRSuperFieldAccess (thisType: JObjectType, field: JField) extends IRFieldAccess {
    def modifyEnv (env: Environment): Environment = env
  }

  case class IRStaticFieldAccess (field: JField) extends IRFieldAccess {
    def modifyEnv (env: Environment): Environment = env
  }

  sealed trait IRProcedureCall extends IRExpression {
    def metaArgs: Map[String, MetaArgument]
    def procedure: JProcedure
    def args: List[IRExpression]
    def requiredContexts: List[IRContextRef]

    def modifyEnv (env: Environment): Environment = {
      env.modify(args).activates(activates).deactivates(deactivates)
    }

    lazy val activates: List[IRContextRef] = procedure.activates.traverse(_.bind(metaArgs).collect { case obj: JObjectType => IRContextRef(obj) }).getOrElse {
      errorAndReturn("invalid context type", Nil)
    }

    lazy val deactivates: List[IRContextRef] = procedure.deactivates.traverse(_.bind(metaArgs).collect { case obj: JObjectType => IRContextRef(obj) }).getOrElse {
      errorAndReturn("invalid context type", Nil)
    }

    lazy val throws: List[JType] = procedure.exceptionTypes.flatMap(_.bind(metaArgs))
  }

  case class IRNewExpression(metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRProcedureCall {
    def procedure: JProcedure = constructor
    def staticType = Some(constructor.declaring)
  }

  sealed trait IRMethodCall extends IRProcedureCall {
    def method: JMethod
    def procedure: JProcedure = method

    lazy val staticType: Option[JType] = method.returnType.bind(metaArgs)
  }

  case class IRInstanceMethodCall(instance: IRExpression, metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

  case class IRSuperMethodCall(thisType: JObjectType, metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

  case class IRStaticMethodCall(metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

  case class IRDSLOperation(method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall {
    override def toString: String = "#DSLOperation#" + method.name + args.mkString("(", ",", ")")
  }

  case class IRContextOperation(context: IRContextRef, method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall {
    override def toString: String = "#ContextOperation#" + context.contextType.name + '.' + method.name + args.mkString("(", ",", ")")
  }

  case class IRDefaultArgument(defaultMethod: JMethod) extends IRExpression {
    def staticType: Option[JType] = defaultMethod.returnType.bind(Map.empty)
    def modifyEnv (env: Environment): Environment = env
  }

  case class IRVariableArguments(args: List[IRExpression], componentType: Option[JType]) extends IRExpression {
    def staticType: Option[JType] = componentType.map(_.array)
    def modifyEnv(env: Environment): Environment = env.modify(args)
  }

  sealed trait IRJavaLiteral extends IRExpression with IRAnnotationElement {
    def modifyEnv (env: Environment): Environment = env
  }

  sealed trait IRClassLiteral extends IRJavaLiteral

  case class IRObjectClassLiteral (clazz: JClass, dim: Int) extends IRClassLiteral {
    def staticType: Option[JObjectType] = getObjectType(clazz, Nil).map(_.array(dim)).flatMap(classTypeOf)
  }

  case class IRPrimitiveClassLiteral (primitiveType: JPrimitiveType, dim: Int) extends IRClassLiteral {
    def staticType: Option[JObjectType] = boxedPrimitiveTypes(primitiveType).map(_.array(dim)).flatMap(classTypeOf)
  }

  case class IRCharLiteral (value: Char) extends IRJavaLiteral {
    def staticType = Some(charType)
  }

  case class IRIntLiteral(value: Int) extends IRJavaLiteral {
    def staticType = Some(intType)
  }

  case class IRLongLiteral(value: Long) extends IRJavaLiteral {
    def staticType = Some(longType)
  }

  case class IRBooleanLiteral(value: Boolean) extends IRJavaLiteral {
    def staticType = Some(booleanType)
  }

  case class IRStringLiteral(value: String) extends IRJavaLiteral {
    def staticType: Option[JObjectType] = stringType
  }

  case class IRNullLiteral(expected: JType) extends IRJavaLiteral {
    def staticType = Some(expected)
  }

  case class IRThisRef(thisType: JObjectType) extends IRExpression {
    def staticType = Some(thisType)

    def modifyEnv (env: Environment): Environment = env

    override def toString: String = thisType.name + '.' + "this"
  }

  case class IRLocalVariableRef(localType: JType, name: String) extends IRLeftHandSide {
    def staticType = Some(localType)
    def modifyEnv (env: Environment): Environment = env
  }

  case class IRContextRef(contextType: JObjectType) extends IRExpression {
    def staticType = Some(contextType)
    def modifyEnv (env: Environment): Environment = env
  }

  sealed trait IRAnnotationElement

  case class IRAnnotation(annotationClass: JClass, args: Map[String, IRAnnotationElement]) extends IRAnnotationElement

  case class IRAnnotationElementArray(array: List[IRAnnotationElement]) extends IRAnnotationElement

  case class IREnumConstantRef(field: JFieldDef) extends IRAnnotationElement

  case class IRStatementExpression(stmt: IRStatement) extends IRExpression {
    def staticType: Option[JType] = boxedVoidType
    def modifyEnv (env: Environment): Environment = stmt.modifyEnv(env)
  }
}