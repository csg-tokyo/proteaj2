package phenan.prj.generator.ir2sir

import phenan.prj._
import phenan.prj.exception._
import phenan.prj.generator.SimplifiedIRs
import phenan.prj.ir._

import scalaz._
import scalaz.std.list._
import scalaz.std.option._
import scalaz.syntax.traverse._

/**
  * Created by ichikawa on 2017/07/04.
  */
trait SimplifiedIRGeneratorsModule {
  this: SimplifiedIRs with ExpectedTypeInferencer with JTypeLoader with JClassLoader
    with IRs with IRStatements with IRExpressions with JModules with JMembers with JErasedTypes with Application =>

  object SimplifiedIRGenerators {

    def compilationUnit (file: IRFile): SIRFile = {
      SIRFile(file.packageName.map(_.names.mkString(".")), file.topLevelModules.map(moduleDef), file.filePath)
    }

    def moduleDef (module: IRModule): SIRModule = module match {
      case clazz: IRClass         => classDef(clazz)
      case interface: IRInterface => interfaceDef(interface)
      case enum: IREnum           => enumDef(enum)
      case dsl: IRDSL             => dslDef(dsl)
    }

    def classDef (clazz: IRClass): SIRClass = {
      SIRClass(clazz.annotations.map(annotation), clazz.mod, clazz.simpleName, clazz.signature, clazz.declaredPriorities,
        clazz.priorityConstraints, clazz.withDSLs, clazz.declaredMembers.map(classMember) ++ clazz.syntheticMethods.map(syntheticMember))
    }

    def interfaceDef (interface: IRInterface): SIRInterface = {
      SIRInterface(interface.annotations.map(annotation), interface.mod, interface.simpleName, interface.signature,
        interface.declaredMembers.map(interfaceMember) ++ interface.syntheticMethods.map(syntheticMember))
    }

    def enumDef (enum: IREnum): SIREnum = {
      SIREnum(enum.annotations.map(annotation), enum.mod, enum.name, enum.signature, enum.enumConstants.map(enumConstant),
        enum.enumMembers.map(enumMember) ++ enum.syntheticMethods.map(syntheticMember))
    }

    def dslDef (dsl: IRDSL): SIRClass = {
      SIRClass(dsl.annotations.map(annotation), dsl.mod, dsl.simpleName, dsl.signature, dsl.declaredPriorities,
        dsl.priorityConstraints, dsl.withDSLs, dsl.declaredMembers.map(dslMember) ++ dsl.syntheticMethods.map(syntheticMember))
    }

    def classMember (member: IRClassMember): SIRMember = member match {
      case field: IRClassField             => fieldDef(field)
      case method: IRClassMethod           => methodDef(method)
      case constructor: IRClassConstructor => constructorDef(constructor)
      case iin: IRClassInstanceInitializer => instanceInitializerDef(iin)
      case sin: IRClassStaticInitializer   => staticInitializerDef(sin)
      case module: IRModule                => moduleDef(module)
    }

    def interfaceMember (member: IRInterfaceMember): SIRMember = member match {
      case field: IRInterfaceField   => fieldDef(field)
      case method: IRInterfaceMethod => methodDef(method)
      case module: IRModule          => moduleDef(module)
    }

    def enumMember (member: IREnumMember): SIRMember = member match {
      case field: IREnumField             => fieldDef(field)
      case method: IREnumMethod           => methodDef(method)
      case constructor: IREnumConstructor => constructorDef(constructor)
      case iin: IREnumInstanceInitializer => instanceInitializerDef(iin)
      case sin: IREnumStaticInitializer   => staticInitializerDef(sin)
      case module: IRModule               => moduleDef(module)
    }

    def dslMember (member: IRDSLMember): SIRMember = member match {
      case field: IRDSLField             => fieldDef(field)
      case operator: IROperator          => operatorDef(operator)
      case constructor: IRDSLConstructor => constructorDef(constructor)
    }

    private def syntheticMember(synthetic: IRSyntheticMethod): SIRMethod = synthetic match {
      case ini: IRFieldInitializer     => fieldInitializer(ini)
      case ini: IRParameterInitializer => parameterInitializer(ini)
    }

    def methodDef (method: IRMethod): SIRMethod = {
      val requires = method.requiresContexts.map(c => c.contextType -> generateLocalName)
      val activates = method.activateTypes.map(c => c -> generateLocalName)

      val methodBody = method.methodBody.map { body =>
        val gen = for {
          _ <- declareLocals(activates)
          _ <- addContexts(requires)
          _ <- addContexts(activates)
          b <- block(body.block)
        } yield b

        val (stmts, blck, _) = gen.run(activates, Map.empty)
        if (method.signature.returnType == VoidTypeSignature &&
          activates.nonEmpty &&
          ! blck.statements.lastOption.exists(_.isInstanceOf[SIRReturnStatement]))
        {
          val returnStatements = mkRetList(activates.head._1, SIRLocalRef(activates.head._2), activates.tail).run(Nil, Map.empty)._1
          SIRBlock(stmts ++ blck.statements ++ returnStatements)
        }
        else SIRBlock(stmts ++ blck.statements)
      }

      SIRMethod(method.annotations.map(annotation), method.mod, method.name, method.syntax, method.signature,
        method.parameters.map(_.name), requires.map(_._2), methodBody)
    }

    def operatorDef (operator: IROperator): SIRMethod = {
      val requires = operator.requiresContexts.map(c => c.contextType -> generateLocalName)
      val activates = operator.activateTypes.map(c => c -> generateLocalName)

      val body = operator.operatorBody.map { body =>
        val gen = for {
          _ <- declareLocals(activates)
          _ <- addContexts(requires)
          _ <- addContexts(activates)
          b <- block(body.block)
        } yield b

        val (stmts, blck, _) = gen.run(activates, Map.empty)
        if (operator.signature.returnType == VoidTypeSignature &&
          activates.nonEmpty &&
          ! blck.statements.lastOption.exists(_.isInstanceOf[SIRReturnStatement]))
        {
          val returnStatements = mkRetList(activates.head._1, SIRLocalRef(activates.head._2), activates.tail).run(Nil, Map.empty)._1
          SIRBlock(stmts ++ blck.statements ++ returnStatements)
        }
        else SIRBlock(stmts ++ blck.statements)
      }

      SIRMethod(operator.annotations.map(annotation), operator.mod, operator.name, operator.syntax, operator.signature,
        operator.parameters.map(_.name), requires.map(_._2), body)
    }

    def fieldDef (field: IRField): SIRField = {
      val fieldInit = field.initializer.map { init =>
        if (field.isStatic) SIRStaticMethodCall(field.declaringClass.classModule, Nil, init.name, Nil)
        else SIRInstanceMethodCall(SIRThisRef(field.declaringClass.thisType), Nil, init.name, Nil)
      }

      SIRField(field.annotations.map(annotation), field.mod, field.name, field.signature, fieldInit)
    }

    def constructorDef (constructor: IRConstructor): SIRConstructor = {
      val requires = constructor.requiresContexts.map(c => c.contextType -> SIRLocalRef(generateLocalName))

      val IRConstructorBody(constructorCall, rest) = constructor.constructorBody

      val gen = for {
        _ <- constructorCall.traverse_(explicitConstructorCall)
        _ <- rest.traverse_(statement)
      } yield ()

      val (stmts, _, _) = gen.run(Nil, requires.toMap)

      SIRConstructor(constructor.annotations.map(annotation), constructor.mod, constructor.declaringClass.simpleName,
        constructor.signature, constructor.parameters.map(_.name), requires.map(_._2.name), SIRBlock(stmts))
    }

    def instanceInitializerDef (ini: IRInstanceInitializer): SIRInstanceInitializer = {
      val (_, blck, _) = block(ini.initializerBody.block).run(Nil, Map.empty)
      SIRInstanceInitializer(blck)
    }

    def staticInitializerDef (ini: IRStaticInitializer): SIRStaticInitializer = {
      val (_, blck, _) = block(ini.initializerBody.block).run(Nil, Map.empty)
      SIRStaticInitializer(blck)
    }

    def enumConstant (const: IREnumConstant): SIREnumConstant = SIREnumConstant(const.name)

    def fieldInitializer (ini: IRFieldInitializer): SIRMethod = {
      val (stmts, value, _) = expression(ini.expression).run(Nil, Map.empty)
      SIRMethod(Nil, ini.mod, ini.name, None, ini.signature, Nil, Nil, Some(SIRBlock(stmts :+ SIRReturnStatement(Some(value)))))
    }

    def parameterInitializer (ini: IRParameterInitializer): SIRMethod = {
      val (stmts, value, _) = expression(ini.expression).run(Nil, Map.empty)
      SIRMethod(Nil, ini.mod, ini.name, None, ini.signature, Nil, Nil, Some(SIRBlock(stmts :+ SIRReturnStatement(Some(value)))))
    }

    def explicitConstructorCall (call: IRExplicitConstructorCall): Gen[Unit] = call match {
      case c: IRThisConstructorCall  => thisConstructorCall(c)
      case c: IRSuperConstructorCall => superConstructorCall(c)
    }

    def thisConstructorCall (call: IRThisConstructorCall): Gen[Unit] = {
      for {
        args     <- call.args.traverse(expression(_).listen)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        _        <- tell(SIRThisConstructorCall(metaArgs(call.metaArgs, call.constructor), explicitConstructorCallArgs(call, args, contexts)))
      } yield ()
    }

    def superConstructorCall (call: IRSuperConstructorCall): Gen[Unit] = {
      for {
        args     <- call.args.traverse(expression(_).listen)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        _        <- tell(SIRSuperConstructorCall(metaArgs(call.metaArgs, call.constructor), explicitConstructorCallArgs(call, args, contexts)))
      } yield ()
    }

    def explicitConstructorCallArgs (call: IRExplicitConstructorCall, args: List[(SIRSimpleExpression, List[SIRStatement])], contexts: List[SIRSimpleExpression]): List[SIRExpression] = {
      inferArgTypes(call).zip(args).map { case (t, (expr, stmts)) =>
        SIRStaticMethodCall(supplierRunnerClass.classModule, List(t), "run", List(SIRLambda(Nil, SIRBlock(stmts :+ SIRReturnStatement(Some(expr))))))
      } ++ contexts
    }

    def inferArgTypes (call: IRExplicitConstructorCall): List[JRefType] = {
      call.constructor.parameterTypes.map { param =>
        inferExpectedType(param.actualGenericType, call.metaArgs, call.constructor).map(boxing).getOrElse {
          throw InvalidTypeException(s"invalid argument type for constructor of ${call.constructor.declaringClass.name}")
        }
      }
    }


    def statement (stmt: IRStatement): Gen[Unit] = stmt match {
      case IRLocalDeclarationStatement(declaration) => localDeclaration(declaration)
      case blck : IRBlock               => block(blck).flatMap(tell)
      case stmt : IRIfStatement         => ifStatement(stmt)
      case stmt : IRForStatement        => forStatement(stmt)
      case stmt : IRWhileStatement      => whileStatement(stmt)
      case stmt : IRTryStatement        => tryStatement(stmt)
      case stmt : IRThrowStatement      => throwStatement(stmt)
      case stmt : IRActivateStatement   => activateStatement(stmt)
      case stmt : IRReturnStatement     => returnStatement(stmt)
      case stmt : IRExpressionStatement => expressionStatement(stmt)
    }

    def localDeclaration (local: IRLocalDeclaration): Gen[Unit] = {
      val IRLocalDeclaration(localType, declarators) = local
      declarators.traverse_[Gen] { case IRVariableDeclarator(name, dim, init) =>
        for {
          expr <- init.traverse(expression)
          _    <- tell(SIRLocalDeclaration(localType.array(dim), name, expr))
        } yield ()
      }
    }

    def block (blck: IRBlock): Gen[SIRBlock] = {
      for {
        body <- blck.statements.traverse_[Gen](statement).written
      } yield SIRBlock(body)
    }

    def ifStatement (stmt: IRIfStatement): Gen[Unit] = {
      for {
        condExpr <- expression(stmt.condition)
        thenStmt <- statement(stmt.thenStatement).written
        elseStmt <- stmt.elseStatement.traverse(statement(_).written)
        _        <- tell(SIRIfStatement(condExpr, SIRBlock(thenStmt), elseStmt.map(SIRBlock)))
      } yield ()
    }

    def forStatement (stmt: IRForStatement): Gen[Unit] = stmt match {
      case stmt: IRNormalForStatement   => normalForStatement(stmt)
      case stmt: IRAncientForStatement  => ancientForStatement(stmt)
      case stmt: IREnhancedForStatement => enhancedForStatement(stmt)
    }

    def normalForStatement (stmt: IRNormalForStatement): Gen[Unit] = {
      for {
        _      <- localDeclaration(stmt.local)
        pair   <- expression(stmt.condition.getOrElse(IRBooleanLiteral(true))).listen
        update <- stmt.update.traverse_[Gen](expression(_).discard).written
        body   <- statement(stmt.statement).written
        _      <- tell(buildWhileStatement(pair._2, pair._1, body ++ update))
      } yield ()
    }

    def ancientForStatement (stmt: IRAncientForStatement): Gen[Unit] = {
      for {
        _      <- stmt.init.traverse_[Gen](expression(_).discard)
        pair   <- expression(stmt.condition.getOrElse(IRBooleanLiteral(true))).listen
        update <- stmt.update.traverse_[Gen](expression(_).discard).written
        body   <- statement(stmt.statement).written
        _      <- tell(buildWhileStatement(pair._2, pair._1, body ++ update))
      } yield ()
    }

    def enhancedForStatement (stmt: IREnhancedForStatement): Gen[Unit] = {
      for {
        body     <- statement(stmt.statement).written
        iterable <- expression(stmt.header.iterable)
        _        <- tell(SIREnhancedForStatement(stmt.header.elementType.array(stmt.header.dim), stmt.header.name, iterable, SIRBlock(body)))
      } yield ()
    }

    def whileStatement (stmt: IRWhileStatement): Gen[Unit] = {
      for {
        zip  <- expression(stmt.condition).listen
        body <- statement(stmt.statement).written
        _    <- tell(buildWhileStatement(zip._2, zip._1, body))
      } yield ()
    }

    private def buildWhileStatement (update: List[SIRStatement], cond: SIRSimpleExpression, body: List[SIRStatement]): SIRWhileStatement = {
      SIRWhileStatement(
        SIRBooleanLiteral(true),
        SIRBlock(update :+ SIRIfStatement(cond, SIRBlock(body), Some(SIRBlock(List(SIRBreakStatement)))))
      )
    }

    def tryStatement (stmt: IRTryStatement): Gen[Unit] = {
      for {
        tryBlock   <- block(stmt.tryBlock)
        catches    <- stmt.catchBlocks.traverse(exceptionHandler)
        finalBlock <- stmt.finallyBlock.traverse(block)
        _          <- tell(SIRTryStatement(tryBlock, catches, finalBlock))
      } yield ()
    }

    def exceptionHandler (handler: IRExceptionHandler): Gen[SIRExceptionHandler] = {
      for {
        catchBlock <- block(handler.catchBlock)
      } yield SIRExceptionHandler(handler.header.exceptionType, handler.header.name, catchBlock)
    }

    def throwStatement (stmt: IRThrowStatement): Gen[Unit] = {
      for {
        exception <- expression(stmt.expression)
        _         <- tell(SIRThrowStatement(exception))
      } yield ()
    }

    def activateStatement (stmt: IRActivateStatement): Gen[Unit] = {
      stmt.expression.staticType match {
        case contextType: JObjectType =>
          for {
            ref  <- lookup(contextType)
            expr <- expression(stmt.expression)
            _    <- tell(SIRExpressionStatement(SIRAssignmentExpression(ref, expr)))
          } yield ()
        case _ =>
          throw InvalidASTException("activates non object type")
      }
    }

    def returnStatement (stmt : IRReturnStatement): Gen[Unit] = {
      for {
        expr      <- expression(stmt.expression)
        activates <- ask
        _         <- mkRetList(stmt.expression.staticType, expr, activates)
      } yield ()
    }

    def expressionStatement (stmt: IRExpressionStatement): Gen[Unit] = {
      for ( _ <- expression(stmt.expression) ) yield ()
    }

    def expression (expr: IRExpression): Gen[SIRSimpleExpression] = expr match {
      case IRStatementExpression(stmt) => statement(stmt).map(_ => SIRNullLiteral)

      case arg : IRScopeArgument      => scopeArgument(arg)
      case arg : IRContextualArgument => contextSensitiveArgument(arg)
      case arg : IRVariableArguments  => variableArgument(arg)
      case arg : IRDefaultArgument    => defaultArgument(arg)

      case call : IRMethodCall    => methodCall(call)
      case call : IRNewExpression => constructorCall(call)
      case access : IRFieldAccess => fieldAccess(access)

      case newArr : IRNewArray          => newArray(newArr)
      case arrInit : IRArrayInitializer => arrayInit(arrInit)
      case access : IRArrayAccess       => arrayAccess(access)

      case cast : IRCastExpression         => castExpression(cast)
      case assign : IRAssignmentExpression => assignmentExpression(assign)

      case IRLocalVariableRef(_, name) => ret(SIRLocalRef(name))
      case IRContextRef(t)             => lookup(t).cast
      case IRThisRef(t)                => ret(SIRThisRef(t))
      case literal : IRJavaLiteral     => ret(javaLiteral(literal))

      case anonClass : IRAnonymousClass => ???
    }

    /* foo(bar(baz(), hoge()))
     *     ~~~~~~~~~~~~~~~~~~
     *
     * Context c;
     * Baz x = baz();
     * c = ActivatedContext.get(0);
     * Hoge y = c.hoge();
     * Bar z = bar(x, y);
     * ---
     * z
     */
    def scopeArgument (scopeArg : IRScopeArgument): Gen[SIRSimpleExpression] = {
      val IRScopeArgument(arg, scopes) = scopeArg
      val cs = scopes.map(c => c.contextType -> generateLocalName)

      for {
        _ <- tell(cs.map { case (typ, name) => SIRLocalDeclaration(scopedContextTypeOf(typ), name, Some(SIRConstructorCall(List(typ), scopedContextTypeOf(typ), Nil))) })
        _ <- modify(_ ++ cs.map { case (typ, name) => typ -> SIRInstanceFieldAccess(SIRLocalRef(name), "context") })
        e <- expression(arg)
      } yield e
    }

    /* foo(bar(baz(), hoge()))
     *     ~~~~~~~~~~~~~~~~~~
     *
     * Context -> Bar w = (Context c) -> {
     *   Baz x = c.baz();
     *   Hoge y = c.hoge();
     *   Bar z = bar(x, y);
     *   return z;
     * };
     * ---
     * w
     */
    def contextSensitiveArgument (csArg: IRContextualArgument): Gen[SIRSimpleExpression] = {
      val IRContextualArgument(arg, contexts) = csArg
      val cs = contexts.map(c => c.contextType -> generateLocalName)
      val argType = contexts.foldRight(arg.staticType)((c, t) => functionTypeOf(c.contextType, t))

      val bodyGen = for {
        _    <- addContexts(cs)
        expr <- expression(arg)
        _    <- tell(SIRReturnStatement(Some(expr)))
      } yield ()

      for {
        body <- bodyGen.written
        ref  <- write(argType, SIRLambda(cs, SIRBlock(body)))
      } yield ref
    }

    def variableArgument (vArgs: IRVariableArguments): Gen[SIRSimpleExpression] = vArgs.componentType match {
      case p: JPrimitiveType =>
        for {
          args <- vArgs.args.traverse(expression)
          init <- ret(SIRArrayInit(p, 1, args))
          ref  <- write(vArgs.staticType, init)
        } yield ref
      case r: JRefType =>
        for {
          args <- vArgs.args.traverse(expression)
          call <- ret(SIRStaticMethodCall(arraysUtilClass.classModule, List(r), "mkArray", args))
          ref  <- write(vArgs.staticType, call)
        } yield ref
    }

    def defaultArgument (arg: IRDefaultArgument): Gen[SIRSimpleExpression] = {
      write(arg.staticType, SIRStaticMethodCall(arg.defaultMethod.declaringClass.classModule, metaArgs(arg.metaArgs, arg.defaultMethod), arg.defaultMethod.name, Nil))
    }

    def methodCall (call: IRMethodCall): Gen[SIRSimpleExpression] = call match {
      case m: IRInstanceMethodCall => instanceMethodCall(m)
      case m: IRStaticMethodCall   => staticMethodCall(m)
      case m: IRSuperMethodCall    => superMethodCall(m)
      case m: IRDSLOperation       => dslOperatorCall(m)
      case m: IRContextOperation   => contextOperatorCall(m)
    }

    def instanceMethodCall (call: IRInstanceMethodCall): Gen[SIRSimpleExpression] = {
      for {
        receiver <- expression(call.instance)
        args     <- call.args.traverse(expression)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        ref      <- writeProcCall(call.staticType, call.activates, SIRInstanceMethodCall(receiver, metaArgs(call.metaArgs, call.method), call.method.name, args ++ contexts))
      } yield ref
    }

    def staticMethodCall (call: IRStaticMethodCall): Gen[SIRSimpleExpression] = {
      for {
        args     <- call.args.traverse(expression)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        ref      <- writeProcCall(call.staticType, call.activates, SIRStaticMethodCall(call.method.declaringClass.classModule, metaArgs(call.metaArgs, call.method), call.method.name, args ++ contexts))
      } yield ref
    }

    def superMethodCall (call: IRSuperMethodCall): Gen[SIRSimpleExpression] = {
      for {
        args     <- call.args.traverse(expression)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        ref      <- writeProcCall(call.staticType, call.activates, SIRSuperMethodCall(call.thisType, metaArgs(call.metaArgs, call.method), call.method.name, args ++ contexts))
      } yield ref
    }

    def dslOperatorCall (call: IRDSLOperation): Gen[SIRSimpleExpression] = {
      for {
        args     <- call.args.traverse(expression)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        ref      <- writeProcCall(call.staticType, call.activates, SIRStaticMethodCall(call.method.declaringClass.classModule, metaArgs(call.metaArgs, call.method), call.method.name, args ++ contexts))
      } yield ref
    }

    def contextOperatorCall (call: IRContextOperation): Gen[SIRSimpleExpression] = {
      for {
        receiver <- expression(call.context)
        args     <- call.args.traverse(expression)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        ref      <- writeProcCall(call.staticType, call.activates, SIRInstanceMethodCall(receiver, metaArgs(call.metaArgs, call.method), call.method.name, args ++ contexts))
      } yield ref
    }

    def constructorCall (call: IRNewExpression): Gen[SIRSimpleExpression] = {
      if (call.activates.nonEmpty) throw new RuntimeException("constructor that has activates clause is not currently supported")
      for {
        args     <- call.args.traverse(expression)
        contexts <- call.requiredContexts.map(_.contextType).traverse(lookup)
        ref      <- write(call.staticType, SIRConstructorCall(metaArgs(call.metaArgs, call.constructor), call.constructor.declaring, args ++ contexts))
      } yield ref
    }

    def fieldAccess (access: IRFieldAccess): Gen[SIRSimpleExpression] = access match {
      case f: IRInstanceFieldAccess => instanceFieldAccess(f)
      case f: IRStaticFieldAccess   => staticFieldAccess(f)
      case f: IRSuperFieldAccess    => superFieldAccess(f)
    }

    def instanceFieldAccess (access: IRInstanceFieldAccess): Gen[SIRSimpleExpression] = {
      for ( receiver <- expression(access.instance) ) yield SIRInstanceFieldAccess(receiver, access.field.name)
    }

    def staticFieldAccess (access: IRStaticFieldAccess): Gen[SIRSimpleExpression] = {
      ret(SIRStaticFieldAccess(access.field.declaringClass.classModule, access.field.name))
    }

    def superFieldAccess (access: IRSuperFieldAccess): Gen[SIRSimpleExpression] = {
      ret(SIRSuperFieldAccess(access.thisType, access.field.name))
    }

    def newArray (newArr: IRNewArray): Gen[SIRSimpleExpression] = {
      for {
        len <- newArr.length.traverse(expression)
        ref <- write(newArr.staticType, SIRNewArray(newArr.componentType, len, newArr.dim))
      } yield ref
    }

    def arrayInit (arrInit: IRArrayInitializer): Gen[SIRSimpleExpression] = {
      for {
        args <- arrInit.components.traverse(expression)
        ref  <- write(arrInit.staticType, SIRArrayInit(arrInit.componentType, arrInit.dim, args))
      } yield ref
    }

    def arrayAccess (access: IRArrayAccess): Gen[SIRSimpleExpression] = {
      for {
        array <- expression(access.array)
        index <- expression(access.index)
      } yield SIRArrayAccess(array, index)
    }

    def castExpression (cast: IRCastExpression): Gen[SIRSimpleExpression] = {
      for {
        expr <- expression(cast.expression)
        ref  <- write(cast.staticType, SIRCastExpression(cast.destType, expr))
      } yield ref
    }

    def assignmentExpression (assign: IRAssignmentExpression): Gen[SIRSimpleExpression] = {
      for {
        left  <- expression(assign.left)
        right <- expression(assign.right)
        ref   <- write(assign.staticType, SIRAssignmentExpression(left, right))
      } yield ref
    }

    def javaLiteral (literal: IRJavaLiteral): SIRJavaLiteral = literal match {
      case IRObjectClassLiteral(clazz, dim)  => SIRObjectClassLiteral(clazz, dim)
      case IRPrimitiveClassLiteral(prm, dim) => SIRPrimitiveClassLiteral(prm, dim)
      case IRCharLiteral(value)    => SIRCharLiteral(value)
      case IRIntLiteral(value)     => SIRIntLiteral(value)
      case IRLongLiteral(value)    => SIRLongLiteral(value)
      case IRBooleanLiteral(value) => SIRBooleanLiteral(value)
      case IRStringLiteral(value)  => SIRStringLiteral(value)
      case IRNullLiteral(_)        => SIRNullLiteral
    }

    def annotation (ann: IRAnnotation): SIRAnnotation = {
      SIRAnnotation(ann.annotationClass, ann.args.mapValues(annotationElement))
    }

    def annotationElement (elem: IRAnnotationElement): SIRAnnotationElement = elem match {
      case lit: IRJavaLiteral              => javaLiteral(lit)
      case ann: IRAnnotation               => annotation(ann)
      case IRAnnotationElementArray(array) => SIRAnnotationElementArray(array.map(annotationElement))
      case IREnumConstantRef(field)        => SIREnumConstantRef(field)
    }

    private def metaArgs (metaArgs: Map[String, MetaArgument], procedure: JProcedure): List[MetaArgument] = {
      procedure.methodDef.signature.metaParams.map { param =>
        metaArgs.getOrElse(param.name, throw InvalidASTException("invalid type argument for " + param.name))
      }
    }

    private type Gen[T] = RWS[List[(JObjectType, String)], List[SIRStatement], Map[JObjectType, SIRSimpleExpression], T]

    private def ret [T] (t: T): Gen[T] = RWS((_, s) => (Nil, t, s))

    // for reader
    private def ask: Gen[List[(JObjectType, String)]] = ReaderT.ask[Id.Id, List[(JObjectType, String)]].rwst

    // for writer
    private def tell (stmt: SIRStatement): Gen[Unit] = WriterT.tell[List[SIRStatement]](List(stmt)).rwst

    private def tell (stmts: List[SIRStatement]): Gen[Unit] = WriterT.tell[List[SIRStatement]](stmts).rwst

    private def write (t: JType, e: SIRExpression): Gen[SIRSimpleExpression] = {
      val generatedName = generateLocalName
      tell(SIRLocalDeclaration(t, generatedName, Some(e))).map(_ => SIRLocalRef(generatedName))
    }

    private def declareLocals (ts: List[(JObjectType, String)]): Gen[Unit] = ts.traverse_[Gen] {
      case (t, name) => tell(SIRLocalDeclaration(t, name, None))
    }

    private def writeProcCall (returns: JType, activates: List[IRContextRef], e: SIRExpression): Gen[SIRSimpleExpression] = {
      val activateTypes = activates.map(_.contextType)
      if (returns == voidType) {
        if (activateTypes.isEmpty) for {
          _ <- tell(SIRExpressionStatement(e))
        } yield SIRDummyExpression
        else for {
          ref <- write(mkReturnTypeList(activateTypes.head, activateTypes.tail), e)
          _   <- deconstructReturnContexts(ref, activateTypes.head, activateTypes.tail)
        } yield SIRDummyExpression
      }
      else {
        if (activateTypes.isEmpty) for {
          ref <- write(returns, e)
        } yield ref
        else for {
          list <- write(mkReturnTypeList(returns, activateTypes), e)
          ref  <- write(returns, SIRInstanceFieldAccess(list, "_1"))
          rest <- write(mkReturnTypeList(activateTypes.head, activateTypes.tail), SIRInstanceFieldAccess(list, "_2"))
          _    <- deconstructReturnContexts(rest, activateTypes.head, activateTypes.tail)
        } yield ref
      }
    }

    private def mkReturnTypeList (returnType: JType, types: List[JType]): JType = types match {
      case head :: tail => pairTypeOf(returnType, mkReturnTypeList(head, tail))
      case Nil => returnType
    }

    private def deconstructReturnContexts (ref: SIRSimpleExpression, head: JObjectType, tail: List[JObjectType]): Gen[Unit] = tail match {
      case car :: cdr => for {
        _    <- assignOrDeclareContext(head, SIRInstanceFieldAccess(ref, "_1"))
        ref2 <- write(mkReturnTypeList(car, cdr), SIRInstanceFieldAccess(ref, "_2"))
        _    <- deconstructReturnContexts(ref2, car, cdr)
      } yield ()
      case Nil => assignOrDeclareContext(head, ref)
    }

    private def mkRetList (t1: JType, ref1: SIRSimpleExpression, list: List[(JObjectType, String)]): Gen[Unit] = list match {
      case (t2, name) :: tail => for {
        ref  <- write(pairTypeOf(t1, t2), SIRStaticMethodCall(pairClass.classModule, List(boxing(t1), t2), "mkPair", List(ref1, SIRLocalRef(name))))
        ans  <- mkRetList(pairTypeOf(t1, t2), ref, tail)
      } yield ans
      case Nil => tell(SIRReturnStatement(Some(ref1)))
    }

    private def generateLocalName: String = "ProteaJLocalVariable$$" + generateUniqueId

    private implicit class GenCovariant [T] (gen: Gen[T]) {
      def cast [U >: T] : Gen[U] = gen.map(t => t)

      def discard : Gen[Unit] = gen.map(_ => ())

      def listen: Gen[(T, List[SIRStatement])] = RWS { (r, s) =>
        val (w, a, s2) = gen.run(r, s)
        (Nil, (a, w), s2)
      }
    }

    private implicit class GenUnit (gen: Gen[Unit]) {
      def written: Gen[List[SIRStatement]] = RWS { (r, s) =>
        val (w, _, s2) = gen.run(r, s)
        (Nil, w, s2)
      }
    }

    // for state
    private def lookup (t: JObjectType): Gen[SIRSimpleExpression] = State.gets[Map[JObjectType, SIRSimpleExpression], SIRSimpleExpression](_(t)).rwst

    private def modify (f: Map[JObjectType, SIRSimpleExpression] => Map[JObjectType, SIRSimpleExpression]): Gen[Unit] = State.modify(f).rwst

    private def addContexts (contexts: List[(JObjectType, String)]): Gen[Unit] = {
      modify(_ ++ contexts.map { case (typ, str) => (typ, SIRLocalRef(str)) })
    }

    private def assignOrDeclareContext (t: JObjectType, expr: SIRSimpleExpression): Gen[Unit] = RWS { (_, s) =>
      if (s.contains(t)) (List(SIRExpressionStatement(SIRAssignmentExpression(s(t), expr))), (), s)
      else {
        val generatedName = generateLocalName
        (List(SIRLocalDeclaration(t, generatedName, Some(expr))), (), s + (t -> SIRLocalRef(generatedName)))
      }
    }
  }
}
