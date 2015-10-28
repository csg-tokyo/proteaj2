package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.Memo._

trait JavaExpressionParsers {
  this: ArgumentParsers with ExpressionParsers with TypeParsers with CommonParsers with TwoLevelParsers =>

  def getJavaExpressionParser (env: Environment): HParser[IRExpression] = cached(env).expression
  def getExplicitConstructorCallParser (env: Environment): HParser[IRExplicitConstructorCall] = cached(env).explicitConstructorCall

  trait JavaExpressionParsersInterface {
    def expression: HParser[IRExpression]
    def explicitConstructorCall: HParser[IRExplicitConstructorCall]
  }

  private val cached: Environment => JavaExpressionParsersInterface = mutableHashMapMemo(new JavaExpressionParsersImpl(_))

  private class JavaExpressionParsersImpl (env: Environment) extends JavaExpressionParsersInterface {
    lazy val expression: HParser[IRExpression] = assignment | arrayCreation | primary | cast

    lazy val assignment: HParser[IRAssignmentExpression] = simpleAssignment   // | += | -= | ...

    lazy val simpleAssignment: HParser[IRSimpleAssignmentExpression] = leftHandSide <~ '=' >> { left =>
      left.staticType match {
        case Some(t) => getExpressionParser(t, env) ^^ { right => IRSimpleAssignmentExpression(left, right) }
        case None    => HParser.failure("type error")
      }
    }

    lazy val leftHandSide: HParser[IRLeftHandSide] = primary ^? { case e: IRLeftHandSide => e }

    lazy val arrayCreation: HParser[IRArrayCreation] = newArray | arrayInitializer

    lazy val newArray: HParser[IRNewArray] = ( "new" ~> typeParsers.componentType ) ~ ( '[' ~> intExpression <~ ']' ).+ ~ dimension ^^ {
      case componentType ~ length ~ dim => IRNewArray(componentType, length, dim)
    }

    lazy val arrayInitializer: HParser[IRArrayInitializer] = ( "new" ~> typeParsers.componentType ) ~ dimension1 >> {
      case componentType ~ dim => '{' ~> getExpressionParser(componentType.array(dim - 1), env).*(',') <~ ','.? <~ '}' ^^ {
        case components => IRArrayInitializer(componentType, dim, components)
      }
    }

    lazy val primary: HParser[IRExpression] = methodCall | fieldAccess | arrayAccess | newExpression | abbreviatedMethodCall | classLiteral | variableRef | thisRef | abbreviatedFieldAccess | parenthesized

    lazy val newExpression: HParser[IRNewExpression] = ( "new" ~> typeParsers.metaArguments ) ~ typeParsers.objectType >>? {
      case metaArgs ~ constructType => constructType.findConstructor(env.clazz).flatMap(constructorCall(metaArgs, _)).reduceOption(_ | _)
    }

    lazy val anonymousClass: HParser[IRAnonymousClass] = ( "new" ~> typeParsers.metaArguments ) ~ typeParsers.objectType >>? {
      case metaArgs ~ baseType => anonymousClass_Constructors(metaArgs, baseType, baseType.findConstructor(env.clazz))
    }

    private def anonymousClass_Constructors (metaArgs: List[MetaArgument], baseType: JObjectType, constructors: List[JConstructor]): Option[HParser[IRAnonymousClass]] = constructors match {
      case Nil if metaArgs.isEmpty => Some(anonymousClassBody ^^ { IRAnonymousClass(Map.empty, baseType, Nil, Nil, _) })
      case cs                      => cs.flatMap { anonymousClass_Constructor(metaArgs, baseType, _) }.reduceOption(_ | _)
    }

    private def anonymousClass_Constructor (metaArgs: List[MetaArgument], baseType: JObjectType, constructor: JConstructor): Option[HParser[IRAnonymousClass]] = {
      anonymousClass_Arguments(metaArgs, constructor).map { argParser =>
        argParser ~ anonymousClassBody ^^ { case (ma, args, cs) ~ members => IRAnonymousClass(ma, baseType, args, cs, members) }
      }
    }

    private def anonymousClass_Arguments (metaArgs: List[MetaArgument], constructor: JConstructor): Option[HParser[(Map[String, MetaArgument], List[IRExpression], List[IRContextRef])]] = {
      if (constructor.parameterTypes.isEmpty && metaArgs.isEmpty) Some(( '(' ~> ')' ) .? ^^^ (Map.empty[String, MetaArgument], Nil, Nil))
      else procedureArguments(constructor, metaArgs)
    }

    lazy val anonymousClassBody: HParser[List[IRClassMember]] = '{' ~> anonymousClassMember.* <~ '}'

    lazy val anonymousClassMember: HParser[IRClassMember] = ???

    lazy val explicitConstructorCall: HParser[IRExplicitConstructorCall] = thisConstructorCall | superConstructorCall

    lazy val thisConstructorCall: HParser[IRThisConstructorCall] = typeParsers.metaArguments <~ "this" >>? { metaArgs =>
      env.thisType.flatMap { _.findConstructor(env.clazz).flatMap(thisConstructorCall(metaArgs, _)).reduceOption(_ | _) }
    }

    lazy val superConstructorCall: HParser[IRSuperConstructorCall] = typeParsers.metaArguments <~ "super" >>? { metaArgs =>
      env.thisType.flatMap(_.superType).flatMap { _.findConstructor(env.clazz).flatMap(superConstructorCall(metaArgs, _)).reduceOption(_ | _) }
    }

    lazy val methodCall: HParser[IRMethodCall] = staticMethodCall | superMethodCall | instanceMethodCall

    lazy val abbreviatedMethodCall: HParser[IRMethodCall] = thisClassMethodCall | thisMethodCall

    lazy val instanceMethodCall: HParser[IRInstanceMethodCall] = primary ~ ( '.' ~> typeParsers.metaArguments ) ~ identifier >>? {
      case instance ~ metaArgs ~ name => instance.staticType.flatMap { _.findMethod(name, env.clazz, isThisRef(instance)).flatMap(invokeVirtual(instance, metaArgs, _)).reduceOption(_ | _) }
    }

    lazy val superMethodCall: HParser[IRSuperMethodCall] = ( "super" ~> '.' ~> typeParsers.metaArguments ) ~ identifier >>? {
      case metaArgs ~ name => env.thisType.flatMap { thisType =>
        thisType.superType.flatMap(_.findMethod(name, env.clazz, true).flatMap(invokeSpecial(thisType, metaArgs, _)).reduceOption(_ | _))
      }
    }

    lazy val staticMethodCall: HParser[IRStaticMethodCall] = typeParsers.className ~ ( '.' ~> typeParsers.metaArguments ) ~ identifier >>? {
      case clazz ~ metaArgs ~ name => clazz.classModule.findMethod(name, env.clazz).flatMap(invokeStatic(metaArgs, _)).reduceOption(_ | _)
    }

    lazy val thisMethodCall: HParser[IRInstanceMethodCall] = identifier >>? { name =>
      env.thisType.flatMap { self => self.findMethod(name, env.clazz, true).map(invokeVirtual(IRThisRef(self), _)).reduceOption(_ | _) }
    }

    lazy val thisClassMethodCall: HParser[IRStaticMethodCall] = identifier >>? { name =>
      env.clazz.classModule.findMethod(name, env.clazz).map(invokeStatic).reduceOption(_ | _)
    }

    lazy val fieldAccess: HParser[IRFieldAccess] = staticFieldAccess | superFieldAccess | instanceFieldAccess

    lazy val abbreviatedFieldAccess: HParser[IRFieldAccess] =  thisClassFieldAccess | thisFieldAccess  // | staticImported

    lazy val instanceFieldAccess: HParser[IRInstanceFieldAccess] = primary ~ ( '.' ~> identifier ) ^^? {
      case instance ~ name => instance.staticType.flatMap { _.findField(name, env.clazz, isThisRef(instance)).map(IRInstanceFieldAccess(instance, _)) }
    }

    lazy val superFieldAccess: HParser[IRSuperFieldAccess] = "super" ~> '.' ~> identifier ^^? { name =>
      env.thisType.flatMap { thisType => thisType.superType.flatMap(_.findField(name, env.clazz, true).map(IRSuperFieldAccess(thisType, _))) }
    }

    lazy val staticFieldAccess: HParser[IRStaticFieldAccess] = typeParsers.className ~ ( '.' ~> identifier ) ^^? {
      case clazz ~ name => clazz.classModule.findField(name, env.clazz).map(IRStaticFieldAccess)
    }

    lazy val thisFieldAccess: HParser[IRInstanceFieldAccess] = identifier ^^? { name =>
      env.thisType.flatMap { self => self.findField(name, env.clazz, true).map(IRInstanceFieldAccess(IRThisRef(self), _)) }
    }

    lazy val thisClassFieldAccess: HParser[IRStaticFieldAccess] = identifier ^^? { name =>
      env.clazz.classModule.findField(name, env.clazz).map(IRStaticFieldAccess)
    }

    lazy val arrayAccess: HParser[IRArrayAccess] = primary ~ ( '[' ~> intExpression <~ ']' ) ^^ {
      case array ~ index => IRArrayAccess(array, index)
    }

    lazy val cast: HParser[IRCastExpression] = ( '(' ~> typeParsers.typeName <~ ')' ) ~ primary ^^ {
      case dest ~ expr => IRCastExpression(dest, expr)
    }

    lazy val parenthesized: HParser[IRExpression] = '(' ~> expression <~ ')'

    lazy val classLiteral: HParser[IRClassLiteral] = objectClassLiteral | primitiveClassLiteral

    lazy val objectClassLiteral: HParser[IRObjectClassLiteral] = typeParsers.className ~ dimension <~ '.' <~ "class" ^^ {
      case clazz ~ dim => IRObjectClassLiteral(clazz, dim)
    }

    lazy val primitiveClassLiteral: HParser[IRPrimitiveClassLiteral] = typeParsers.primitiveTypeName ~ dimension <~ '.' <~ "class" ^^ {
      case prm ~ dim => IRPrimitiveClassLiteral(prm, dim)
    }

    lazy val thisRef: HParser[IRThisRef] = "this" ^^? { _ => thisObject }

    lazy val variableRef: HParser[IRLocalVariableRef] = identifier ^^? { env.localVariable }

    lazy val intExpression = getExpressionParser(compiler.typeLoader.int, env)

    lazy val dimension = ( '[' ~> ']' ).* ^^ { _.length }

    lazy val dimension1 = ( '[' ~> ']' ).+ ^^ { _.length }

    private def procedureCall [T] (procedure: JProcedure)(f: (Map[String, MetaArgument], List[IRExpression], List[IRContextRef]) => T): HParser[T] = {
      getArgumentsParser(procedure, env) ^^? { case (bind, args) => env.inferContexts(procedure, bind).map(f(bind, args, _)) }
    }

    private def procedureCall [T] (procedure: JProcedure, metaArgs: List[MetaArgument])(f: (Map[String, MetaArgument], List[IRExpression], List[IRContextRef]) => T): Option[HParser[T]] = {
      if (metaArgs.isEmpty) Some(procedureCall(procedure)(f))
      else for {
        bind     <- binding(procedure, metaArgs)
        contexts <- env.inferContexts(procedure, bind)
      } yield getArgumentsParser(procedure, bind, env) ^^ { f(bind, _, contexts) }
    }

    private def procedureArguments (procedure: JProcedure, metaArgs: List[MetaArgument]): Option[HParser[(Map[String, MetaArgument], List[IRExpression], List[IRContextRef])]] = {
      if (metaArgs.isEmpty) Some(getArgumentsParser(procedure, env) ^^? { case (bind, args) => env.inferContexts(procedure, bind).map((bind, args, _)) })
      else for {
        bind     <- binding(procedure, metaArgs)
        contexts <- env.inferContexts(procedure, bind)
      } yield getArgumentsParser(procedure, bind, env) ^^ { (bind, _, contexts) }
    }

    private def constructorCall (metaArgs: List[MetaArgument], constructor: JConstructor): Option[HParser[IRNewExpression]] =
      procedureCall(constructor, metaArgs) { IRNewExpression(_, constructor, _, _) }

    private def thisConstructorCall (metaArgs: List[MetaArgument], constructor: JConstructor): Option[HParser[IRThisConstructorCall]] =
      procedureCall(constructor, metaArgs) { IRThisConstructorCall(_, constructor, _, _) }

    private def superConstructorCall (metaArgs: List[MetaArgument], constructor: JConstructor): Option[HParser[IRSuperConstructorCall]] =
      procedureCall(constructor, metaArgs) { IRSuperConstructorCall(_, constructor, _, _) }

    private def invokeVirtual (instance: IRExpression, method: JMethod): HParser[IRInstanceMethodCall] =
      procedureCall(method) { IRInstanceMethodCall(instance, _, method, _, _) }

    private def invokeVirtual (instance: IRExpression, metaArgs: List[MetaArgument], method: JMethod): Option[HParser[IRInstanceMethodCall]] =
      procedureCall(method, metaArgs) { IRInstanceMethodCall(instance, _, method, _, _) }

    private def invokeSpecial (thisType: JObjectType, metaArgs: List[MetaArgument], method: JMethod): Option[HParser[IRSuperMethodCall]] =
      procedureCall(method, metaArgs) { IRSuperMethodCall(thisType, _, method, _, _) }

    private def invokeStatic (method: JMethod): HParser[IRStaticMethodCall] =
      procedureCall(method) { IRStaticMethodCall(_, method, _, _) }

    private def invokeStatic (metaArgs: List[MetaArgument], method: JMethod): Option[HParser[IRStaticMethodCall]] =
      procedureCall(method, metaArgs) { IRStaticMethodCall(_, method, _, _) }

    private val typeParsers = getTypeParsers(env.resolver)

    private def thisObject = env.thisType.map(IRThisRef)

    private def isThisRef (e: IRExpression) = thisObject.contains(e)

    private def binding (procedure: JProcedure, metaArgs: List[MetaArgument]): Option[Map[String, MetaArgument]] = {
      val metaParams = procedure.methodDef.signature.metaParams
      if (compiler.typeLoader.validTypeArgs(metaParams, metaArgs, procedure.env)) Some(metaParams.map(_.name).zip(metaArgs).toMap)
      else None
    }
  }
}
