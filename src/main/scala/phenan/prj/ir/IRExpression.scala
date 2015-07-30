package phenan.prj.ir

import phenan.prj._

sealed trait IRExpression {
  def staticType: Option[JType]
  def activates: List[IRContextRef]
  def deactivates: List[IRContextRef]
}

sealed trait IRLeftHandSide extends IRExpression

sealed trait IRAssignmentExpression extends IRExpression {
  def left: IRLeftHandSide
  def right: IRExpression
  def staticType: Option[JType] = right.staticType
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

case class IRSimpleAssignmentExpression (left: IRLeftHandSide, right: IRExpression) extends IRAssignmentExpression

case class IRNewExpression (metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExpression {
  def staticType = Some(constructor.declaring)
  lazy val activates: List[IRContextRef] = IRContextRef.createRefs(constructor.activates, metaArgs).getOrElse {
    constructor.compiler.state.errorAndReturn("invalid context type", Nil)
  }
  lazy val deactivates: List[IRContextRef] = IRContextRef.createRefs(constructor.deactivates, metaArgs).getOrElse {
    constructor.compiler.state.errorAndReturn("invalid context type", Nil)
  }
}

sealed trait IRExplicitConstructorCall

case class IRThisConstructorCall (metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExplicitConstructorCall
case class IRSuperConstructorCall (metaArgs: Map[String, MetaArgument], constructor: JConstructor, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRExplicitConstructorCall

sealed trait IRArrayCreation extends IRExpression

case class IRNewArray (componentType: JType, length: List[IRExpression], dim: Int) extends IRArrayCreation {
  def staticType = Some(componentType.array(length.size + dim))
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

case class IRArrayInitializer (componentType: JType, dim: Int, components: List[IRExpression]) extends IRArrayCreation {
  def staticType = Some(componentType.array(dim))
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

case class IRCastExpression (destType: JType, expression: IRExpression) extends IRExpression {
  def staticType = Some(destType)
  def activates: List[IRContextRef] = expression.activates
  def deactivates: List[IRContextRef] = expression.deactivates
}

case class IRArrayAccess (array: IRExpression, index: IRExpression) extends IRLeftHandSide {
  def staticType = array.staticType match {
    case Some(JArrayType(component)) => Some(component)
    case _ => None
  }
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

sealed trait IRFieldAccess extends IRLeftHandSide {
  def field: JField
  def staticType = Some(field.fieldType)
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

case class IRInstanceFieldAccess (instance: IRExpression, field: JField) extends IRFieldAccess

case class IRSuperFieldAccess (superType: JObjectType, field: JField) extends IRFieldAccess

case class IRStaticFieldAccess (field: JField) extends IRFieldAccess

sealed trait IRMethodCall extends IRExpression {
  def metaArgs: Map[String, MetaArgument]
  def method: JMethod
  lazy val staticType: Option[JType] = method.returnType.bind(metaArgs)
  lazy val activates: List[IRContextRef] = IRContextRef.createRefs(method.activates, metaArgs).getOrElse {
    method.compiler.state.errorAndReturn("invalid context type", Nil)
  }
  lazy val deactivates: List[IRContextRef] = IRContextRef.createRefs(method.deactivates, metaArgs).getOrElse {
    method.compiler.state.errorAndReturn("invalid context type", Nil)
  }
}

case class IRInstanceMethodCall (instance: IRExpression, metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRSuperMethodCall (superType: JObjectType, metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRStaticMethodCall (metaArgs: Map[String, MetaArgument], method: JMethod, args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRDSLOperation (method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRContextOperation (context: IRContextRef, method: JMethod, metaArgs: Map[String, MetaArgument], args: List[IRExpression], requiredContexts: List[IRContextRef]) extends IRMethodCall

case class IRVariableArguments (args: List[IRExpression], componentType: Option[JType]) extends IRExpression {
  def staticType: Option[JType] = componentType.map(_.array)
  def activates: List[IRContextRef] = args.flatMap(_.activates)
  def deactivates: List[IRContextRef] = args.flatMap(_.deactivates)
}

sealed trait IRJavaLiteral extends IRExpression with IRAnnotationElement {
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

sealed trait IRClassLiteral extends IRJavaLiteral

case class IRObjectClassLiteral (clazz: JClass, dim: Int) extends IRClassLiteral {
  def staticType = clazz.objectType(Nil).map(_.array(dim)).flatMap(clazz.compiler.typeLoader.classTypeOf)
}

case class IRPrimitiveClassLiteral (primitiveClass: JPrimitiveType, dim: Int) extends IRClassLiteral {
  def staticType = primitiveClass.boxed.map(_.array(dim)).flatMap(primitiveClass.compiler.typeLoader.classTypeOf)
}

case class IRCharLiteral (value: Char, compiler: JCompiler) extends IRJavaLiteral {
  def staticType = Some(compiler.typeLoader.char)
}

case class IRIntLiteral (value: Int, compiler: JCompiler) extends IRJavaLiteral {
  def staticType = Some(compiler.typeLoader.int)
}

case class IRLongLiteral (value: Long, compiler: JCompiler) extends IRJavaLiteral {
  def staticType = Some(compiler.typeLoader.long)
}

case class IRBooleanLiteral (value: Boolean, compiler: JCompiler) extends IRJavaLiteral {
  def staticType = Some(compiler.typeLoader.boolean)
}

case class IRStringLiteral (value: String, compiler: JCompiler) extends IRJavaLiteral {
  def staticType = compiler.typeLoader.stringType
}

case class IRThisRef (thisType: JObjectType) extends IRExpression {
  def staticType = Some(thisType)
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

case class IRLocalVariableRef (localType: JType, name: String) extends IRLeftHandSide {
  def staticType = Some(localType)
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

case class IRContextRef (contextType: JObjectType) extends IRExpression {
  def staticType = Some(contextType)
  def activates: List[IRContextRef] = Nil
  def deactivates: List[IRContextRef] = Nil
}

object IRContextRef {
  def createRefs (gts: List[JGenericType], bind: Map[String, MetaArgument]): Option[List[IRContextRef]] = createRefs(gts, bind, Nil)

  def createRefsFromSignatures (ss: List[JTypeSignature], bind: Map[String, MetaArgument], compiler: JCompiler): Option[List[IRContextRef]] = createRefsFromSignatures(ss, bind, Nil, compiler)

  private def createRefs (gts: List[JGenericType], bind: Map[String, MetaArgument], refs: List[IRContextRef]): Option[List[IRContextRef]] = gts match {
    case gt :: rest => gt.bind(bind).collect { case obj: JObjectType => IRContextRef(obj) } match {
      case Some(ref) => createRefs(rest, bind, ref :: refs)
      case None      => None
    }
    case Nil => Some(refs.reverse)
  }

  private def createRefsFromSignatures (ss: List[JTypeSignature], bind: Map[String, MetaArgument], refs: List[IRContextRef], compiler: JCompiler): Option[List[IRContextRef]] = ss match {
    case sig :: rest => compiler.typeLoader.fromTypeSignature_RefType(sig, bind).collect { case obj: JObjectType => IRContextRef(obj)} match {
      case Some(ref) => createRefsFromSignatures(rest, bind, ref :: refs, compiler)
      case None      => None
    }
    case Nil => Some(refs.reverse)
  }
}


sealed trait IRAnnotationElement

case class IRAnnotation (annotationClass: JClass, args: Map[String, IRAnnotationElement]) extends IRAnnotationElement

case class IRAnnotationElementArray (array: List[IRAnnotationElement]) extends IRAnnotationElement

case class IREnumConstantRef (field: JFieldDef) extends IRAnnotationElement