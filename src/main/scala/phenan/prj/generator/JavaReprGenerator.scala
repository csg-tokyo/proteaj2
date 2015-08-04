package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._
import phenan.prj.exception._
import phenan.util._

import CommonNames._
import JavaRepr._

object JavaReprGenerator {
  /* AST transformation : ProteaJ IR ==> Java AST */

  def javaFile (file: IRFile): JavaFile = JavaFile (file.packageName.map(_.names.mkString(".")), file.topLevelModules.map(moduleDef))

  def moduleDef (clazz: IRModule): ModuleDef = clazz match {
    case cls: IRTopLevelClass     => Union[ModuleDef](classDef(cls))
    case enm: IRTopLevelEnum      => Union[ModuleDef](enumDef(enm))
    case ifc: IRTopLevelInterface => Union[ModuleDef](interfaceDef(ifc))
    case dsl: IRTopLevelDSL       => Union[ModuleDef](dslDef(dsl))
  }

  def classDef (clazz: IRClass): ClassDef = new ClassDef {
    def annotations = Annotations.classAnnotations(clazz)
    def modifiers = clazz.mod
    def name = clazz.simpleName
    def typeParameters = typeParams(clazz.signature.metaParams)
    def superType = classSig(clazz.signature.superClass)
    def interfaces = clazz.signature.interfaces.map(classSig)
    def members = clazz.declaredMembers.map(classMember) ++ clazz.syntheticMethods.map(syntheticMember)
  }

  def enumDef (enum: IREnum): EnumDef = new EnumDef {
    def annotations = Annotations.enumAnnotations(enum)
    def modifiers = enum.mod
    def name = enum.simpleName
    def interfaces = enum.signature.interfaces.map(classSig)
    def constants = enum.enumConstants.map(enumConstantDef)
    def members = enum.enumMembers.map(enumMember)
  }

  def interfaceDef (interface: IRInterface): InterfaceDef = new InterfaceDef {
    def annotations = Annotations.interfaceAnnotations(interface)
    def modifiers = interface.mod
    def name = interface.simpleName
    def typeParameters = typeParams(interface.signature.metaParams)
    def superInterfaces = interface.signature.interfaces.map(classSig)
    def members = interface.declaredMembers.map(interfaceMember)
  }

  def dslDef (dsl: IRDSL): ClassDef = new ClassDef {
    def annotations = Annotations.dslAnnotations(dsl)
    def modifiers = dsl.mod
    def name = dsl.simpleName
    def typeParameters = Nil
    def superType = objectClassSig
    def interfaces = Nil
    def members = dsl.declaredMembers.flatMap(dslMember) ++ dsl.syntheticMethods.map(syntheticMember)
  }

  def contextDef (context: IRContext): ClassDef = new ClassDef {
    def annotations = Annotations.contextAnnotations(context)
    def modifiers = context.mod
    def name = context.simpleName
    def typeParameters = typeParams(context.signature.metaParams)
    def superType = objectClassSig
    def interfaces = Nil
    def members = context.declaredMembers.map(contextMember) ++ context.syntheticMethods.map(syntheticMember)
  }

  def classMember (member: IRClassMember): ClassMember = member match {
    case field: IRClassField             => Union[ClassMember](fieldDef(field))
    case method: IRClassMethod           => Union[ClassMember](methodDef(method))
    case constructor: IRClassConstructor => Union[ClassMember](constructorDef(constructor))
    case iin: IRClassInstanceInitializer => Union[ClassMember](instanceInitializerDef(iin))
    case sin: IRClassStaticInitializer   => Union[ClassMember](staticInitializerDef(sin))
    case module: IRModule                => Union[ClassMember](moduleDef(module))
  }

  def enumMember (member: IREnumMember): ClassMember = member match {
    case field: IREnumField             => Union[ClassMember](fieldDef(field))
    case method: IREnumMethod           => Union[ClassMember](methodDef(method))
    case constructor: IREnumConstructor => Union[ClassMember](constructorDef(constructor))
    case iin: IREnumInstanceInitializer => Union[ClassMember](instanceInitializerDef(iin))
    case sin: IREnumStaticInitializer   => Union[ClassMember](staticInitializerDef(sin))
    case module: IRModule               => Union[ClassMember](moduleDef(module))
    case _ => throw InvalidASTException("invalid enum declaration AST")
  }

  def interfaceMember (member: IRInterfaceMember): ClassMember = member match {
    case field: IRInterfaceField   => Union[ClassMember](fieldDef(field))
    case method: IRInterfaceMethod => Union[ClassMember](methodDef(method))
    case module: IRModule          => Union[ClassMember](moduleDef(module))
  }

  def dslMember (member: IRDSLMember): Option[ClassMember] = member match {
    case field: IRDSLField       => Some(Union[ClassMember](fieldDef(field)))
    case operator: IRDSLOperator => Some(Union[ClassMember](operatorDef(operator)))
    case context: IRContext      => Some(Union[ClassMember](Union[ModuleDef](contextDef(context))))
    case _: IRDSLPriorities      => None
  }

  def contextMember (member: IRContextMember): ClassMember = member match {
    case field: IRContextField             => Union[ClassMember](fieldDef(field))
    case operator: IRContextOperator       => Union[ClassMember](operatorDef(operator))
    case constructor: IRContextConstructor => Union[ClassMember](constructorDef(constructor))
  }

  def syntheticMember (synthetic: IRSyntheticMethod): ClassMember = synthetic match {
    case ini: IRParameterInitializer => Union[ClassMember](parameterInitializerDef(ini))
  }

  def fieldDef (field: IRField): FieldDef = new FieldDef {
    def annotations = Annotations.fieldAnnotations(field)
    def modifiers = field.mod
    def fieldType = typeSig(field.signature)
    def name = field.name
    def initializer: Option[Expression] = field.initializer.map(expression(_, Nil))
  }

  def methodDef (method: IRMethod): MethodDef = new MethodDef {
    def annotations = Annotations.methodAnnotations(method)
    def modifiers = method.mod
    def typeParameters = typeParams(method.signature.metaParams)
    def returnType = typeSig(method.signature.returnType)
    def name = method.name
    def parameters = method.parameters.map(parameter)
    def throws = method.signature.throwTypes.map(typeSig)
    def body = method.methodBody.map(methodBody(_, method.requiresContexts, method.activateTypes))
  }

  def operatorDef (operator: IROperator): MethodDef = new MethodDef {
    def annotations = Annotations.operatorAnnotations(operator)
    def modifiers = operator.mod
    def typeParameters = typeParams(operator.signature.metaParams)
    def returnType = typeSig(operator.signature.returnType)
    def name = operator.name
    def parameters = operator.parameters.map(parameter)
    def throws = operator.signature.throwTypes.map(typeSig)
    def body = operator.operatorBody.map(methodBody(_, operator.requiresContexts, operator.activateTypes))
  }

  def parameterInitializerDef (initializer: IRParameterInitializer): MethodDef = new MethodDef {
    def annotations = Annotations.paramInitializerAnnotations(initializer)
    def modifiers = initializer.mod
    def typeParameters = Nil
    def returnType = typeSig(initializer.signature.returnType)
    def name = initializer.name
    def parameters = Nil
    def throws = Nil
    def body = initializer.expression.map(parameterInitializer)
  }

  def constructorDef (constructor: IRConstructor): ConstructorDef = new ConstructorDef {
    def annotations = Annotations.constructorAnnotations(constructor)
    def modifiers = constructor.mod
    def typeParameters = typeParams(constructor.signature.metaParams)
    def className = constructor.declaringClass.simpleName
    def parameters = constructor.parameters.map(parameter)
    def throws = constructor.signature.throwTypes.map(typeSig)
    def body = constructor.constructorBody.map(constructorBody(_, constructor.requiresContexts, constructor.activateTypes)).getOrElse {
      throw InvalidASTException("constructor must have its body")
    }
  }

  def instanceInitializerDef (iin: IRInstanceInitializer): InstanceInitializerDef = InstanceInitializerDef {
    iin.initializerBody.map(initializerBody).getOrElse {
      throw InvalidASTException("invalid instance initializer")
    }
  }

  def staticInitializerDef (sin: IRStaticInitializer): StaticInitializerDef = StaticInitializerDef {
    sin.initializerBody.map(initializerBody).getOrElse {
      throw InvalidASTException("invalid static initializer")
    }
  }

  def enumConstantDef (constant: IREnumConstant): EnumConstantDef = EnumConstantDef(constant.name)

  def parameter (param: IRFormalParameter): Param = Param (param.actualTypeSignature.map(typeSig).getOrElse { throw InvalidASTException("invalid parameter type") }, param.name)

  def parameter (paramType: JType, name: String): Param = Param (typeToSig(paramType), name)

  def methodBody (body: IRMethodBody, contexts: List[IRContextRef], activates: List[JRefType]): Block = Block {
    contextDeclarations(contexts, 0) ++ blockStatements(body.block.statements, contexts, activates, Nil)
  }

  def constructorBody (body: IRConstructorBody, contexts: List[IRContextRef], activates: List[JRefType]): Block = body.constructorCall match {
    case Some(c) => Block(Union[Statement](explicitConstructorCall(c, Nil)) :: contextDeclarations(contexts, 0) ++ blockStatements(body.statements, contexts, activates, Nil))
    case None    => Block(contextDeclarations(contexts, 0) ++ blockStatements(body.statements, contexts, activates, Nil))
  }

  def initializerBody (body: IRInitializerBody): Block = block(body.block, Nil, Nil)

  def parameterInitializer (body: IRExpression): Block = Block(List(Union[Statement](ReturnStatement(expression(body, Nil)))))

  /* statements */

  def block (b: IRBlock, contexts: List[IRContextRef], activates: List[JRefType]): Block = Block(blockStatements(b.statements, contexts, activates, Nil))

  def blockStatements (statements: List[IRStatement], contexts: List[IRContextRef], activates: List[JRefType], result: List[Statement]): List[Statement] = statements match {
    case (l: IRLocalDeclarationStatement) :: rest => blockStatements(rest, contexts, activates, result :+ Union[Statement](localDeclarationStatement(l, contexts)))
    case (e: IRExpressionStatement) :: rest       => blockStatements(rest, contexts ++ e.activates, activates, result ++ (Union[Statement](expressionStatement(e, contexts)) :: contextDeclarations(e.activates, contexts.size)))
    case single :: rest                           => blockStatements(rest, contexts, activates, result :+ singleStatement(single, contexts, activates))
    case Nil                                      => result
  }

  def singleStatement (statement: IRStatement, contexts: List[IRContextRef], activates: List[JRefType]): Statement = statement match {
    case b: IRBlock               => Union[Statement](block(b, contexts, activates))
    case i: IRIfStatement         => Union[Statement](ifStatement(i, contexts, activates))
    case w: IRWhileStatement      => Union[Statement](whileStatement(w, contexts, activates))
    case f: IRForStatement        => Union[Statement](forStatement(f, contexts, activates))
    case r: IRReturnStatement     => Union[Statement](returnStatement(r, contexts))
    case e: IRExpressionStatement => Union[Statement](expressionStatement(e, contexts))
    case a: IRActivateStatement   => Union[Statement](activateStatement(a, contexts, activates))
    case l: IRLocalDeclarationStatement => throw InvalidASTException("local variable declaration is not a single statement")
  }

  def localDeclarationStatement (stmt: IRLocalDeclarationStatement, contexts: List[IRContextRef]) = LocalDeclarationStatement (localDeclaration(stmt.declaration, contexts))

  def localDeclaration (declaration: IRLocalDeclaration, contexts: List[IRContextRef]) = LocalDeclaration (typeToSig(declaration.localType), declaration.declarators.map(localDeclarator(_, contexts)))

  def localDeclarator (declarator: IRVariableDeclarator, contexts: List[IRContextRef]) = LocalDeclarator (declarator.name, declarator.dim, declarator.init.map(expression(_, contexts)))

  def contextDeclarations (contexts: List[IRContextRef], offset: Int): List[Statement] = contexts.zipWithIndex.map {
    case (c, i) => Union[Statement](contextDeclarationStatement(c, i, offset))
  }

  def contextDeclarationStatement (context: IRContextRef, index: Int, offset: Int) = LocalDeclarationStatement (contextDeclaration(context, index, offset))

  def contextDeclaration (context: IRContextRef, index: Int, offset: Int) = LocalDeclaration (typeToSig(context.contextType), List(contextDeclarator(context.contextType, index, offset)))

  def contextDeclarator (contextType: JObjectType, index: Int, offset: Int): LocalDeclarator = LocalDeclarator (contextName(index + offset), 0, Some(contextAccess(contextType, index)))

  def ifStatement (stmt: IRIfStatement, contexts: List[IRContextRef], activates: List[JRefType]): IfStatement =
    IfStatement (expression(stmt.condition, contexts), singleStatement(stmt.thenStatement, contexts, activates), stmt.elseStatement.map(singleStatement(_, contexts, activates)))

  def whileStatement (stmt: IRWhileStatement, contexts: List[IRContextRef], activates: List[JRefType]): WhileStatement =
    WhileStatement (expression(stmt.condition, contexts), singleStatement(stmt.statement, contexts, activates))

  def forStatement (stmt: IRForStatement, contexts: List[IRContextRef], activates: List[JRefType]): ForStatement = stmt match {
    case s: IRNormalForStatement   => Union[ForStatement](normalForStatement(s, contexts, activates))
    case s: IRAncientForStatement  => Union[ForStatement](ancientForStatement(s, contexts, activates))
    case s: IREnhancedForStatement => Union[ForStatement](enhancedForStatement(s, contexts, activates))
  }

  def normalForStatement (stmt: IRNormalForStatement, contexts: List[IRContextRef], activates: List[JRefType]): NormalForStatement =
    NormalForStatement (Union[ForInit](localDeclaration(stmt.local, contexts)), stmt.condition.map(expression(_, contexts)), stmt.update.map(expression(_, contexts)), singleStatement(stmt.statement, contexts, activates))

  def ancientForStatement (stmt: IRAncientForStatement, contexts: List[IRContextRef], activates: List[JRefType]): NormalForStatement =
    NormalForStatement (Union[ForInit](stmt.init.map(expression(_, contexts))), stmt.condition.map(expression(_, contexts)), stmt.update.map(expression(_, contexts)), singleStatement(stmt.statement, contexts, activates))

  def enhancedForStatement (stmt: IREnhancedForStatement, contexts: List[IRContextRef], activates: List[JRefType]): EnhancedForStatement =
    EnhancedForStatement (typeToSig(stmt.elementType), stmt.name, stmt.dim, expression(stmt.iterable, contexts), singleStatement(stmt.statement, contexts, activates))

  def returnStatement (stmt: IRReturnStatement, contexts: List[IRContextRef]): ReturnStatement = ReturnStatement(expression(stmt.expression, contexts))

  def expressionStatement (stmt: IRExpressionStatement, contexts: List[IRContextRef]): ExpressionStatement = ExpressionStatement(expression(stmt.expression, contexts))

  def activateStatement (stmt: IRActivateStatement, contexts: List[IRContextRef], activates: List[JRefType]): ExpressionStatement = {
    val activateType = stmt.expression.staticType.getOrElse(throw InvalidASTException("invalid activate statement"))
    val index = activates.indexOf(activateType)
    if (index >= 0) activateContext(expression(stmt.expression, contexts), index)
    else throw InvalidASTException("activated context is not declared in the activates clause")
  }

  def explicitConstructorCall (constructorCall: IRExplicitConstructorCall, contexts: List[IRContextRef]): ExplicitConstructorCall = constructorCall match {
    case c: IRThisConstructorCall  => Union[ExplicitConstructorCall](thisConstructorCall(c, contexts))
    case c: IRSuperConstructorCall => Union[ExplicitConstructorCall](superConstructorCall(c, contexts))
  }

  def thisConstructorCall (constructorCall: IRThisConstructorCall, contexts: List[IRContextRef]): ThisConstructorCall = {
    if (constructorCall.requiredContexts.nonEmpty) ???
    else ThisConstructorCall(typeArgs(constructorCall.constructor, constructorCall.metaArgs), constructorCall.args.map(expression(_, contexts)))
  }

  def superConstructorCall (constructorCall: IRSuperConstructorCall, contexts: List[IRContextRef]): SuperConstructorCall = {
    if (constructorCall.requiredContexts.nonEmpty) ???
    else SuperConstructorCall(typeArgs(constructorCall.constructor, constructorCall.metaArgs), constructorCall.args.map(expression(_, contexts)))
  }

  private def contextAccess (contextType: JObjectType, index: Int): Expression = Union[Expression](MethodCall(activatedContextRef, List(Union[TypeArg](typeToSig(contextType))), "get", List(intLiteral(index))))

  private def activateContext (context: Expression, index: Int): ExpressionStatement = ExpressionStatement(Union[Expression](MethodCall(activatedContextRef, Nil, "set", List(intLiteral(index), context))))

  private def intLiteral (value: Int): Expression = Union[Expression](Union[JavaLiteral](Literal(value)))

  private def contextName (index: Int): String = "ProteaJLocalContext$$" + index

  private def activatedContextRef = Union[Receiver](ClassRef("proteaj.lang.ActivatedContexts"))

  /* expressions */

  def expression (expression: IRExpression, contexts: List[IRContextRef]): Expression = expression match {
    case e: IRAssignmentExpression => Union[Expression](assignment(e, contexts))
    case e: IRMethodCall           => Union[Expression](methodCall(e, contexts))
    case e: IRFieldAccess          => Union[Expression](fieldAccess(e, contexts))
    case e: IRNewExpression        => newExpression(e, contexts)
    case e: IRNewArray             => Union[Expression](newArray(e, contexts))
    case e: IRArrayInitializer     => Union[Expression](arrayInit(e, contexts))
    case e: IRArrayAccess          => Union[Expression](arrayAccess(e, contexts))
    case e: IRCastExpression       => Union[Expression](castExpression(e, contexts))
    case IRLocalVariableRef(_, n)  => Union[Expression](LocalRef(n))
    case e: IRThisRef              => Union[Expression](ThisRef(objectType(e.thisType)))
    case e: IRJavaLiteral          => Union[Expression](javaLiteral(e))
    case e: IRVariableArguments    => Union[Expression](variableArguments(e, contexts))
    case e: IRContextRef           => Union[Expression](contextRef(e, contexts))
  }

  def assignment (e: IRAssignmentExpression, contexts: List[IRContextRef]): Assignment = e match {
    case e: IRSimpleAssignmentExpression => simpleAssignment(e, contexts)
  }

  def simpleAssignment (e: IRSimpleAssignmentExpression, contexts: List[IRContextRef]): SimpleAssignment = SimpleAssignment(expression(e.left, contexts), expression(e.right, contexts))

  def fieldAccess (e: IRFieldAccess, contexts: List[IRContextRef]): FieldAccess = e match {
    case IRInstanceFieldAccess(expr, field)  => FieldAccess(Union[Receiver](expression(expr, contexts)), field.name)
    case IRStaticFieldAccess(field)          => FieldAccess(Union[Receiver](ClassRef(field.declaringClass.name)), field.name)
    case IRSuperFieldAccess(thisType, field) => FieldAccess(Union[Receiver](SuperRef(objectType(thisType))), field.name)
  }

  def methodCall (e: IRMethodCall, contexts: List[IRContextRef]): MethodCall = e match {
    case m: IRInstanceMethodCall => instanceMethodCall(m.instance, m, contexts)
    case m: IRStaticMethodCall   => staticMethodCall(m, contexts)
    case m: IRSuperMethodCall    => superMethodCall(m, contexts)
    case op: IRContextOperation  => instanceMethodCall(op.context, op, contexts)
    case op: IRDSLOperation      => staticMethodCall(op, contexts)
  }

  def instanceMethodCall (instance: IRExpression, e: IRMethodCall, contexts: List[IRContextRef]): MethodCall = {
    if (e.requiredContexts.nonEmpty) {
      val lam = instanceMethodWrapper(getStaticType(e), getStaticType(instance), typeArgs(e.method, e.metaArgs), e.method.name, e.args.map(getStaticType), e.throws, e.requiredContexts, contexts)
      MethodCall(Union[Receiver](lam), Nil, "apply", expression(instance, contexts) :: e.args.map(expression(_, contexts)))
    }
    else MethodCall(Union[Receiver](expression(instance, contexts)), typeArgs(e.method, e.metaArgs), e.method.name, e.args.map(expression(_, contexts)))
  }

  def staticMethodCall (e: IRMethodCall, contexts: List[IRContextRef]): MethodCall = {
    if (e.requiredContexts.nonEmpty) {
      val lam = staticMethodWrapper(getStaticType(e), ClassRef(e.method.declaringClass.name), typeArgs(e.method, e.metaArgs), e.method.name, e.args.map(getStaticType), e.throws, e.requiredContexts, contexts)
      MethodCall(Union[Receiver](lam), Nil, "apply", e.args.map(expression(_, contexts)))
    }
    else MethodCall(Union[Receiver](ClassRef(e.method.declaringClass.name)), typeArgs(e.method, e.metaArgs), e.method.name, e.args.map(expression(_, contexts)))
  }

  def superMethodCall (e: IRSuperMethodCall, contexts: List[IRContextRef]): MethodCall = {
    if (e.requiredContexts.nonEmpty) {
      val lam = superMethodWrapper(getStaticType(e), SuperRef(objectType(e.thisType)), typeArgs(e.method, e.metaArgs), e.method.name, e.args.map(getStaticType), e.throws, e.requiredContexts, contexts)
      MethodCall(Union[Receiver](lam), Nil, "apply", e.args.map(expression(_, contexts)))
    }
    else MethodCall(Union[Receiver](SuperRef(objectType(e.thisType))), typeArgs(e.method, e.metaArgs), e.method.name, e.args.map(expression(_, contexts)))
  }

  def newExpression (e: IRNewExpression, contexts: List[IRContextRef]): Expression = {
    if (e.requiredContexts.nonEmpty) {
      val lam = newExpressionWrapper(e.constructor.declaring, typeArgs(e.constructor, e.metaArgs), e.args.map(getStaticType), e.throws, e.requiredContexts, contexts)
      Union[Expression](MethodCall(Union[Receiver](lam), Nil, "apply", e.args.map(expression(_, contexts))))
    }
    else Union[Expression](NewExpression(typeArgs(e.constructor, e.metaArgs), objectType(e.constructor.declaring), e.args.map(expression(_, contexts))))
  }

  def newArray (e: IRNewArray, contexts: List[IRContextRef]): NewArray = NewArray(typeToSig(e.componentType), e.length.map(expression(_, contexts)), e.dim)

  def arrayInit (e: IRArrayInitializer, contexts: List[IRContextRef]): ArrayInit = ArrayInit(typeToSig(e.componentType), e.dim, e.components.map(expression(_, contexts)))

  def arrayAccess (e: IRArrayAccess, contexts: List[IRContextRef]): ArrayAccess = ArrayAccess(expression(e.array, contexts), expression(e.index, contexts))

  def castExpression (e: IRCastExpression, contexts: List[IRContextRef]): CastExpression = CastExpression(typeToSig(e.destType), expression(e.expression, contexts))

  def variableArguments (e: IRVariableArguments, contexts: List[IRContextRef]): ArrayInit =
    ArrayInit(typeToSig(e.componentType.getOrElse { throw InvalidASTException("invalid component type of variable argument") }), 1, e.args.map(expression(_, contexts)))

  def contextRef (e: IRContextRef, contexts: List[IRContextRef]): LocalRef = {
    val index = contexts.indexOf(e)
    if (index >= 0) LocalRef(contextName(index))
    else throw InvalidASTException("context not found")
  }

  private def getStaticType (e: IRExpression): JType = e.staticType.getOrElse {
    throw InvalidASTException("invalid expression : compiler cannot determine the static type of " + e)
  }

  private def typeArgs (procedure: JProcedure, metaArgs: Map[String, MetaArgument]): List[TypeArg] = procedure.metaParameters.map { case (name, _) =>
    metaArgs.get(name).flatMap(metaArgument).getOrElse {
      throw InvalidASTException("invalid type argument for " + name)
    }
  }.toList

  private def instanceMethodWrapper (returnType: JType, receiverType: JType, typeArgs: List[TypeArg], name: String, argTypes: List[JType], throws: List[JType], required: List[IRContextRef], contexts: List[IRContextRef]): Expression =
    functionWrapper(returnType, parameter(receiverType, "receiver") :: argTypes.zipWithIndex.map { case (t, i) => parameter(t, "arg" + i) }, throws, required, contexts,
      instanceMethodCallExpression(localRefExpression("receiver"), typeArgs, name, argTypes.indices.map(i => localRefExpression("arg" + i)).toList))

  private def staticMethodWrapper (returnType: JType, receiver: ClassRef, typeArgs: List[TypeArg], name: String, argTypes: List[JType], throws: List[JType], required: List[IRContextRef], contexts: List[IRContextRef]): Expression =
    functionWrapper(returnType, argTypes.zipWithIndex.map { case (t, i) => parameter(t, "arg" + i) }, throws, required, contexts,
      staticMethodCallExpression(receiver, typeArgs, name, argTypes.indices.map(i => localRefExpression("arg" + i)).toList))

  private def superMethodWrapper (returnType: JType, receiver: SuperRef, typeArgs: List[TypeArg], name: String, argTypes: List[JType], throws: List[JType], required: List[IRContextRef], contexts: List[IRContextRef]): Expression =
    functionWrapper(returnType, argTypes.zipWithIndex.map { case (t, i) => parameter(t, "arg" + i) }, throws, required, contexts,
      superMethodCallExpression(receiver, typeArgs, name, argTypes.indices.map(i => localRefExpression("arg" + i)).toList))

  private def newExpressionWrapper (returnType: JObjectType, typeArgs: List[TypeArg], argTypes: List[JType], throws: List[JType], required: List[IRContextRef], contexts: List[IRContextRef]): Expression =
    functionWrapper(returnType, argTypes.zipWithIndex.map { case (t, i) => parameter(t, "arg" + i) }, throws, required, contexts,
      Union[Expression](NewExpression(typeArgs, objectType(returnType), argTypes.indices.map(i => localRefExpression("arg" + i)).toList)))

  private def functionWrapper (returnType: JType, params: List[Param], throws: List[JType], required: List[IRContextRef], contexts: List[IRContextRef], application: Expression): Expression = Union[Expression](lambda (
    typeToSig(returnType), params, throws.map(typeToSig), Block { sendRequiredContexts(required, contexts) :+ returnStatement(application) }
  ))
  
  private def returnStatement (e: Expression): Statement = Union[Statement](ReturnStatement(e))

  private def localRefExpression (name: String): Expression = Union[Expression](LocalRef(name))

  private def instanceMethodCallExpression (receiver: Expression, typeArgs: List[TypeArg], name: String, args: List[Expression]): Expression = Union[Expression](MethodCall(Union[Receiver](receiver), typeArgs, name, args))
  private def staticMethodCallExpression (receiver: ClassRef, typeArgs: List[TypeArg], name: String, args: List[Expression]): Expression = Union[Expression](MethodCall(Union[Receiver](receiver), typeArgs, name, args))
  private def superMethodCallExpression (receiver: SuperRef, typeArgs: List[TypeArg], name: String, args: List[Expression]): Expression = Union[Expression](MethodCall(Union[Receiver](receiver), typeArgs, name, args))

  private def sendRequiredContexts (required: List[IRContextRef], contexts: List[IRContextRef]) = required.zipWithIndex.map { case (c, i) =>
    Union[Statement](activateContext(Union[Expression](contextRef(c, contexts)), i))
  }

  private def lambda (retType: TypeSig, params: List[Param], exceptions: List[TypeSig], block: Block): AnonymousClass =
    AnonymousClass(objectClassSig, Nil, List(Union[ClassMember](lambdaMethodDef(retType, params, exceptions, block))))

  private def lambdaMethodDef (retType: TypeSig, params: List[Param], exceptions: List[TypeSig], block: Block): MethodDef = new MethodDef {
    def annotations: List[JavaAnnotation] = Nil
    def modifiers: JModifier = JModifier(JModifier.accPublic)
    def typeParameters: List[TypeParam] = Nil
    def returnType = retType
    def name: String = "apply"
    def parameters: List[Param] = params
    def throws: List[TypeSig] = exceptions
    def body = Some(block)
  }

  /* literals */

  def javaLiteral (literal: IRJavaLiteral): JavaLiteral = literal match {
    case c: IRClassLiteral   => Union[JavaLiteral](classLiteral(c))
    case s: IRStringLiteral  => Union[JavaLiteral](Literal(s.value))
    case c: IRCharLiteral    => Union[JavaLiteral](Literal(c.value))
    case i: IRIntLiteral     => Union[JavaLiteral](Literal(i.value))
    case j: IRLongLiteral    => Union[JavaLiteral](Literal(j.value))
    case z: IRBooleanLiteral => Union[JavaLiteral](Literal(z.value))
  }

  def classLiteral (c: IRClassLiteral): ClassLiteral = c match {
    case IRObjectClassLiteral(clazz, d)        => ClassLiteral(clazz.name, d)
    case IRPrimitiveClassLiteral(primitive, d) => ClassLiteral(primitive.name, d)
  }

  def classLiteral (clazz: JClass): ClassLiteral = ClassLiteral(clazz.name, 0)

  /* signatures */

  def typeToSig (t: JType): TypeSig = t match {
    case obj: JObjectType           => Union[TypeSig](objectType(obj))
    case prm: JPrimitiveType        => Union[TypeSig](PrimitiveSig(prm.name))
    case JArrayType(component)      => Union[TypeSig](ArraySig(typeToSig(component)))
    case JTypeVariable(name, _, _)  => Union[TypeSig](TypeVariableSig(name))
    case cap: JCapturedWildcardType => throw InvalidASTException("captured wildcard is found in generated Java AST")
    case unb: JUnboundTypeVariable  => throw InvalidASTException("unbound type variable is found in generated Java AST")
  }

  def objectType (obj: JObjectType): ClassSig = Union[ClassSig](topLevelClassObjectType(obj))

  def topLevelClassObjectType (obj: JObjectType): TopLevelClassSig = TopLevelClassSig (obj.erase.name, obj.erase.signature.metaParams.flatMap { param =>
      metaArgument(obj.env.getOrElse(param.name, throw InvalidASTException("invalid type argument")))
  })

  def metaArgument (arg: MetaArgument): Option[TypeArg] = arg match {
    case ref: JRefType   => Some(Union[TypeArg](typeToSig(ref)))
    case wild: JWildcard => Some(Union[TypeArg](wildcard(wild)))
    case meta: MetaValue => None
  }

  def wildcard (wild: JWildcard): Wildcard = wild match {
    case JWildcard(Some(ub), _) => Union[Wildcard](UpperBoundWildcard(typeToSig(ub)))
    case JWildcard(_, Some(lb)) => Union[Wildcard](LowerBoundWildcard(typeToSig(lb)))
    case JWildcard(None, None)  => Union[Wildcard](UnboundWildcard)
  }

  def typeParams (mps: List[FormalMetaParameter]): List[TypeParam] = mps.filter(_.metaType == JTypeSignature.typeTypeSig).map(typeParam)

  def typeParam (mp: FormalMetaParameter): TypeParam = TypeParam(mp.name, mp.bounds.map(typeSig))

  def typeSig (signature: JTypeSignature): TypeSig = signature match {
    case c: JClassTypeSignature        => Union[TypeSig](classSig(c))
    case p: JPrimitiveTypeSignature    => Union[TypeSig](primitiveSig(p))
    case JTypeVariableSignature(n)     => Union[TypeSig](TypeVariableSig(n))
    case JArrayTypeSignature(c)        => Union[TypeSig](ArraySig(typeSig(c)))
    case c: JCapturedWildcardSignature => throw InvalidASTException("captured wildcard is found in generated Java code")
  }

  def classSig (signature: JClassTypeSignature): ClassSig = signature match {
    case s: SimpleClassTypeSignature => Union[ClassSig](topLevelClassSig(s))
    case m: MemberClassTypeSignature => Union[ClassSig](memberClassSig(m))
  }

  def objectClassSig: ClassSig = classSig(JTypeSignature.objectTypeSig)

  def topLevelClassSig (signature: SimpleClassTypeSignature): TopLevelClassSig = TopLevelClassSig (signature.internalName.replace('/', '.'), signature.args.flatMap(typeArg))

  def memberClassSig (signature: MemberClassTypeSignature): MemberClassSig = MemberClassSig (classSig(signature.outer), signature.clazz, signature.args.flatMap(typeArg))

  def primitiveSig (signature: JPrimitiveTypeSignature): PrimitiveSig = signature match {
    case ByteTypeSignature   => PrimitiveSig("byte")
    case CharTypeSignature   => PrimitiveSig("char")
    case DoubleTypeSignature => PrimitiveSig("double")
    case FloatTypeSignature  => PrimitiveSig("float")
    case IntTypeSignature    => PrimitiveSig("int")
    case LongTypeSignature   => PrimitiveSig("long")
    case ShortTypeSignature  => PrimitiveSig("short")
    case BoolTypeSignature   => PrimitiveSig("boolean")
    case VoidTypeSignature   => PrimitiveSig("void")
  }

  def typeArg (arg: JTypeArgument): Option[TypeArg] = arg match {
    case signature: JTypeSignature      => Some(Union[TypeArg](typeSig(signature)))
    case wild: WildcardArgument         => Some(Union[TypeArg](wildcard(wild)))
    case metaVar: MetaVariableSignature => None
  }

  def wildcard (wild: WildcardArgument): Wildcard = wild match {
    case WildcardArgument(Some(ub), _) => Union[Wildcard](UpperBoundWildcard(typeSig(ub)))
    case WildcardArgument(_, Some(lb)) => Union[Wildcard](LowerBoundWildcard(typeSig(lb)))
    case WildcardArgument(None, None)  => Union[Wildcard](UnboundWildcard)
  }

  /* annotation */

  def annotation (ann: IRAnnotation): JavaAnnotation = JavaAnnotation (ann.annotationClass.name, ann.args.mapValues(annotationElement))

  def annotationElement (e: IRAnnotationElement): AnnotationElement = e match {
    case array: IRAnnotationElementArray => Union[AnnotationElement](elementArray(array))
    case ann: IRAnnotation        => Union[AnnotationElement](annotation(ann))
    case literal: IRJavaLiteral   => Union[AnnotationElement](javaLiteral(literal))
    case const: IREnumConstantRef => Union[AnnotationElement](enumConstRef(const))
  }

  def elementArray (array: IRAnnotationElementArray): ElementArray = ElementArray (array.array.map(annotationElement))

  def enumConstRef (const: IREnumConstantRef): EnumConstRef = EnumConstRef (const.field.declaringClass.name, const.field.name)

  object Annotations {
    def classAnnotations (clazz: IRClass): List[JavaAnnotation] = {
      if (clazz.isDSL) classLikeAnnotations(clazz) :+ dslAnnotation(clazz.declaredPriorities, clazz.priorityConstraints, clazz.withDSLs)
      else classLikeAnnotations(clazz)
    }

    def enumAnnotations (enum: IREnum): List[JavaAnnotation] = classLikeAnnotations(enum)

    def interfaceAnnotations (interface: IRInterface): List[JavaAnnotation] = classLikeAnnotations(interface)

    def dslAnnotations (dsl: IRDSL): List[JavaAnnotation] = {
      classLikeAnnotations(dsl) :+ dslAnnotation(dsl.declaredPriorities, dsl.priorityConstraints, dsl.withDSLs)
    }

    def contextAnnotations (context: IRContext): List[JavaAnnotation] = {
      classLikeAnnotations(context) :+ contextAnnotation
    }

    def fieldAnnotations (field: IRField): List[JavaAnnotation] = {
      except(fieldSigClassName)(field.annotations) :+ fieldSignatureAnnotation(field.signature)
    }

    def methodAnnotations (method: IRMethod): List[JavaAnnotation] = method.syntax match {
      case Some(syntax) => methodLikeAnnotations(method) :+ operatorAnnotation(syntax)
      case None         => methodLikeAnnotations(method)
    }

    def operatorAnnotations (operator: IROperator): List[JavaAnnotation] = {
      methodLikeAnnotations(operator) :+ operatorAnnotation(operator.operatorSyntax)
    }

    def constructorAnnotations (constructor: IRConstructor): List[JavaAnnotation] = methodLikeAnnotations(constructor)

    def paramInitializerAnnotations (initializer: IRParameterInitializer): List[JavaAnnotation] = List(methodSignatureAnnotation(initializer.signature))

    private def classLikeAnnotations (clazz: IRModule): List[JavaAnnotation] = {
      except(classSigClassName, dslClassName, contextClassName)(clazz.annotations) :+ classSignatureAnnotation(clazz.signature)
    }

    private def methodLikeAnnotations (method: IRProcedure): List[JavaAnnotation] = {
      except(methodSigClassName, operatorClassName)(method.annotations) :+ methodSignatureAnnotation(method.signature)
    }

    private def except (names: String*)(as: List[IRAnnotation]): List[JavaAnnotation] = as.filterNot { ann => names.contains(ann.annotationClass.internalName) }.map(annotation)

    private def classSignatureAnnotation (sig: JClassSignature): JavaAnnotation = {
      mkAnnotation(classSigClassName) (
        "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
        "superType" -> strLit(sig.superClass.toString),
        "interfaces" -> array(sig.interfaces.map(cts => strLit(cts.toString)))
      )
    }

    private def methodSignatureAnnotation (sig: JMethodSignature): JavaAnnotation = {
      mkAnnotation(methodSigClassName) (
        "metaParameters" -> array(sig.metaParams.map(metaParameterAnnotation).map(elementAnnotation)),
        "returnType" -> strLit(sig.returnType.toString),
        "parameters" -> array(sig.parameters.map(p => strLit(p.toString))),
        "throwsTypes" -> array(sig.throwTypes.map(s => strLit(s.toString))),
        "activates" -> array(sig.activates.map(s => strLit(s.toString))),
        "deactivates" -> array(sig.deactivates.map(s => strLit(s.toString))),
        "requires" -> array(sig.requires.map(s => strLit(s.toString)))
      )
    }

    private def fieldSignatureAnnotation (sig: JTypeSignature): JavaAnnotation = {
      mkAnnotation(fieldSigClassName) ("value" -> strLit(sig.toString))
    }

    private def dslAnnotation (declaredPriorities: Set[JPriority], priorityConstraints: List[List[JPriority]], withDSLs: List[JClass]): JavaAnnotation = {
      mkAnnotation(dslClassName) (
        "priorities" -> array(declaredPriorities.map(p => strLit(p.name)).toList),
        "constraints" -> array(priorityConstraints.map(constraintAnnotation).map(elementAnnotation)),
        "with" -> array(withDSLs.map(classLit))
      )
    }

    private def contextAnnotation: JavaAnnotation = mkAnnotation(contextClassName)()

    private def operatorAnnotation (syntax: JSyntaxDef): JavaAnnotation = syntax match {
      case JExpressionSyntaxDef(priority, pattern) => operatorAnnotation("Expression", priority, pattern)
      case JLiteralSyntaxDef(priority, pattern)    => operatorAnnotation("Literal", priority, pattern)
      case JStatementSyntaxDef(priority, pattern)  => operatorAnnotation("Statement", priority, pattern)
    }

    private def operatorAnnotation (level: String, priority: JPriority, pattern: List[JSyntaxElementDef]): JavaAnnotation = {
      mkAnnotation(operatorClassName) (
        "level" -> enumConst(opLevelClassName, level),
        "priority" -> elementAnnotation(priorityAnnotation(priority)),
        "pattern" -> array(pattern.map(operatorElementAnnotation).map(elementAnnotation))
      )
    }

    private def metaParameterAnnotation (fmp: FormalMetaParameter): JavaAnnotation = {
      mkAnnotation(metaParamClassName)(
        "name" -> strLit(fmp.name),
        "type" -> strLit(fmp.metaType.toString),
        "priority" -> array(fmp.priority.map(priorityAnnotation).map(elementAnnotation).toList),
        "bounds" -> array(fmp.bounds.map(sig => strLit(sig.toString))))
    }

    private def constraintAnnotation (constraint: List[JPriority]): JavaAnnotation = {
      mkAnnotation(constraintClassName)("value" -> array(constraint.map(priorityAnnotation).map(elementAnnotation)))
    }

    private def priorityAnnotation (priority: JPriority): JavaAnnotation = {
      mkAnnotation(priorityClassName)("dsl" -> strLit(priority.clazz.toString), "name" -> strLit(priority.name))
    }

    private def operatorElementAnnotation (elem: JSyntaxElementDef): JavaAnnotation = elem match {
      case JOperatorNameDef(name) => operatorElementAnnotation("Name", name)
      case JOperandDef            => operatorElementAnnotation("Hole", "")
      case JRepetition0Def        => operatorElementAnnotation("Star", "")
      case JRepetition1Def        => operatorElementAnnotation("Plus", "")
      case JOptionalOperandDef    => operatorElementAnnotation("Optional", "")
      case JAndPredicateDef(sig)  => operatorElementAnnotation("AndPredicate", sig.toString)
      case JNotPredicateDef(sig)  => operatorElementAnnotation("NotPredicate", sig.toString)
      case JMetaValueRefDef(name) => operatorElementAnnotation("Reference", name)
    }

    private def operatorElementAnnotation (name: String, value: String): JavaAnnotation = {
      mkAnnotation(opElemClassName) ( "kind" -> enumConst(opElemTypeClassName, name), name -> strLit(value) )
    }

    private def mkAnnotation (annName: String)(args: (String, AnnotationElement)*): JavaAnnotation = JavaAnnotation(annName, args.toMap)

    private def elementAnnotation (ann: JavaAnnotation) = Union[AnnotationElement](ann)

    private def array (es: List[AnnotationElement]) = Union[AnnotationElement](ElementArray(es))

    private def enumConst (enum: String, const: String) = Union[AnnotationElement](EnumConstRef(enum, const))

    private def strLit (str: String) = Union[AnnotationElement](Union[JavaLiteral](Literal(str)))

    private def classLit (clazz: JClass) = Union[AnnotationElement](Union[JavaLiteral](classLiteral(clazz)))
  }
}
