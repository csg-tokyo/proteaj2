package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

trait JavaExpressionParsersModule {
  this: ArgumentParsersModule with ExpressionParsersModule with TypeParsersModule
    with CommonParsersModule with ContextSensitiveParsersModule
    with JTypeLoader with Environments with EnvModifyStrategy
    with IRs with IRExpressions with JModules with JMembers with JErasedTypes =>

  trait JavaExpressionParsers {
    this: ArgumentParsers with ExpressionParsers with TypeParsers with CommonParsers with ContextSensitiveParsers =>

    def javaExpressionParser: ContextSensitiveParser[IRExpression] = JavaExpressionParsersImpl.expression

    def explicitConstructorCallParser: ContextSensitiveParser[IRExplicitConstructorCall] = JavaExpressionParsersImpl.explicitConstructorCall

    trait JavaExpressionParsersInterface {
      def expression: ContextSensitiveParser[IRExpression]

      def explicitConstructorCall: ContextSensitiveParser[IRExplicitConstructorCall]
    }

    private object JavaExpressionParsersImpl extends JavaExpressionParsersInterface {
      lazy val expression: ContextSensitiveParser[IRExpression] = assignment | arrayCreation | primary | cast

      lazy val assignment: ContextSensitiveParser[IRAssignmentExpression] = simpleAssignment // | += | -= | ...

      lazy val simpleAssignment: ContextSensitiveParser[IRSimpleAssignmentExpression] = leftHandSide <~ '=' >> { left =>
        getExpressionParser(left.staticType) ^^ { right => IRSimpleAssignmentExpression(left, right) }
      }

      lazy val leftHandSide: ContextSensitiveParser[IRLeftHandSide] = primary ^? { case e: IRLeftHandSide => e }

      lazy val arrayCreation: ContextSensitiveParser[IRArrayCreation] = newArray | arrayInitializer

      lazy val newArray: ContextSensitiveParser[IRNewArray] = ("new" ~> typeParsers.componentType) >># { componentType =>
        ('[' ~> intExpression <~ ']').+ >> { length =>
          dimension ^^# { IRNewArray(componentType, length, _) }
        }
      }

      lazy val arrayInitializer: ContextSensitiveParser[IRArrayInitializer] = "new" ~> typeParsers.componentType ~ dimension1 >># {
        case componentType ~ dim =>
          '{' ~> getExpressionParser(componentType.array(dim - 1)).*(',') <~ ','.? <~ '}' ^^ { IRArrayInitializer(componentType, dim, _) }
      }

      lazy val primary: ContextSensitiveParser[IRExpression] = methodCall | fieldAccess | arrayAccess | newExpression | abbreviatedMethodCall | classLiteral.^# | variableRef | thisRef.^# | abbreviatedFieldAccess.^# | parenthesized

      lazy val newExpression: ContextSensitiveParser[IRNewExpression] = ("new" ~> typeParsers.metaArguments) ~ typeParsers.objectType >># {
        case metaArgs ~ constructType =>
          val ps = constructType.findConstructor(declaringModule).map(constructorCall(metaArgs, _))
          ps.foldRight(ContextSensitiveParser.failure[IRNewExpression](""))(_ | _)
      }
      /*
      lazy val anonymousClass: ContextSensitiveParser[IRAnonymousClass] = ("new" ~> typeParsers.metaArguments) ~ typeParsers.objectType >># {
        case metaArgs ~ baseType => anonymousClass_Constructors(metaArgs, baseType)
      }

      private def anonymousClass_Constructors (metaArgs: List[MetaArgument], baseType: JObjectType): ContextSensitiveParser[IRAnonymousClass] = ContextSensitiveParser { env =>
        baseType.findConstructor(env.clazz) match {
          case Nil =>
            anonymousClassBody ^^ { IRAnonymousClass(Map.empty, baseType, Nil, Nil, _) }
          case cs  =>
            val ps = cs.map(anonymousClass_Constructor(metaArgs, baseType, _))
            ps.foldRight(ContextSensitiveParser.failure[IRAnonymousClass](""))(_ | _)
        }
      }

      private def anonymousClass_Constructor (metaArgs: List[MetaArgument], baseType: JObjectType, constructor: JConstructor): ContextSensitiveParser[IRAnonymousClass] = {
        procedureCall(constructor, metaArgs) ~ anonymousClassBody ^^ {
          case (ma, args, cs) ~ members => IRAnonymousClass(ma, baseType, args, cs, members)
        }
      }

      lazy val anonymousClassBody: ContextSensitiveParser[List[IRClassMember]] = '{' ~> anonymousClassMember.* <~ '}'

      lazy val anonymousClassMember: ContextSensitiveParser[IRClassMember] = ???
      */
      lazy val explicitConstructorCall: ContextSensitiveParser[IRExplicitConstructorCall] = thisConstructorCall | superConstructorCall

      lazy val thisConstructorCall: ContextSensitiveParser[IRThisConstructorCall] = typeParsers.metaArguments <~ "this" >># { metaArgs =>
        val ps = for {
          self <- thisType.toList
          constructor <- self.findConstructor(declaringModule)
        } yield thisConstructorCall(metaArgs, constructor)

        ps.foldRight(ContextSensitiveParser.failure[IRThisConstructorCall](""))(_ | _)
      }

      lazy val superConstructorCall: ContextSensitiveParser[IRSuperConstructorCall] = typeParsers.metaArguments <~ "super" >># { metaArgs =>
        val ps = for {
          self <- thisType.toList
          sup  <- self.superType.toList
          constructor <- sup.findConstructor(declaringModule)
        } yield superConstructorCall(metaArgs, constructor)

        ps.foldRight(ContextSensitiveParser.failure[IRSuperConstructorCall](""))(_ | _)
      }

      lazy val methodCall: ContextSensitiveParser[IRMethodCall] = staticMethodCall | superMethodCall | instanceMethodCall

      lazy val abbreviatedMethodCall: ContextSensitiveParser[IRMethodCall] = thisClassMethodCall | thisMethodCall

      lazy val instanceMethodCall: ContextSensitiveParser[IRInstanceMethodCall] = primary >> { instance =>
        ('.' ~> typeParsers.metaArguments) ~ identifier >># { case metaArgs ~ name =>
          val methods = instance.staticType.findMethod(name, declaringModule, thisType.map(IRThisRef).contains(instance))
          methods.map(method => invokeVirtual(instance, metaArgs, method)).foldRight(ContextSensitiveParser.failure[IRInstanceMethodCall](""))(_ | _)
        }
      }

      lazy val superMethodCall: ContextSensitiveParser[IRSuperMethodCall] = ("super" ~> '.' ~> typeParsers.metaArguments) ~ identifier >># {
        case metaArgs ~ name =>
          val ps = for {
            self   <- thisType.toList
            sup    <- self.superType.toList
            method <- sup.findMethod(name, declaringModule, true)
          } yield invokeSpecial(self, metaArgs, method)

          ps.foldRight(ContextSensitiveParser.failure[IRSuperMethodCall](""))(_ | _)
      }

      lazy val staticMethodCall: ContextSensitiveParser[IRStaticMethodCall] = typeParsers.className ~ ('.' ~> typeParsers.metaArguments) ~ identifier >># {
        case clazz ~ metaArgs ~ name =>
          val methods = clazz.classModule.findMethod(name, declaringModule)
          methods.foldRight(ContextSensitiveParser.failure[IRStaticMethodCall](""))(invokeStatic(metaArgs, _) | _)
      }

      lazy val thisMethodCall: ContextSensitiveParser[IRInstanceMethodCall] = identifier >># { name =>
        val ps = for {
          self   <- thisType.toList
          method <- self.findMethod(name, declaringModule, true)
        } yield invokeVirtual(IRThisRef(self), method)

        ps.foldRight(ContextSensitiveParser.failure[IRInstanceMethodCall](""))(_ | _)
      }

      lazy val thisClassMethodCall: ContextSensitiveParser[IRStaticMethodCall] = identifier >># { name =>
        declaringModule.classModule.findMethod(name, declaringModule).map(invokeStatic).foldRight(ContextSensitiveParser.failure[IRStaticMethodCall](""))(_ | _)
      }

      lazy val fieldAccess: ContextSensitiveParser[IRFieldAccess] = staticFieldAccess.^# | superFieldAccess.^# | instanceFieldAccess

      lazy val abbreviatedFieldAccess: ContextFreeParser[IRFieldAccess] = thisClassFieldAccess | thisFieldAccess // | staticImported

      lazy val instanceFieldAccess: ContextSensitiveParser[IRInstanceFieldAccess] = primary >> { instance =>
        ('.' ~> identifier).mapOption { name =>
          val field = instance.staticType.findField(name, declaringModule, thisType.map(IRThisRef).contains(instance))
          field.map(IRInstanceFieldAccess(instance, _))
        }.^#
      }

      lazy val superFieldAccess: ContextFreeParser[IRSuperFieldAccess] = "super" ~> ('.' ~> identifier) ^^? { name =>
        for {
          self  <- thisType
          sup   <- self.superType
          field <- sup.findField(name, declaringModule, true)
        } yield IRSuperFieldAccess(self, field)
      }

      lazy val staticFieldAccess: ContextFreeParser[IRStaticFieldAccess] = typeParsers.className ~ ('.' ~> identifier) ^^? {
        case clazz ~ name => clazz.classModule.findField(name, declaringModule).map(IRStaticFieldAccess)
      }

      lazy val thisFieldAccess: ContextFreeParser[IRInstanceFieldAccess] = identifier ^^? { name =>
        for {
          self  <- thisType
          field <- self.findField(name, declaringModule, true)
        } yield IRInstanceFieldAccess(IRThisRef(self), field)
      }

      lazy val thisClassFieldAccess: ContextFreeParser[IRStaticFieldAccess] = identifier ^^? { name =>
        declaringModule.classModule.findField(name, declaringModule).map(IRStaticFieldAccess)
      }

      lazy val arrayAccess: ContextSensitiveParser[IRArrayAccess] = primary ~ ('[' ~> intExpression <~ ']') ^? {
        case array ~ index if array.staticType.isInstanceOf[JArrayType] => IRArrayAccess(array, index)
      }

      lazy val cast: ContextSensitiveParser[IRCastExpression] = ('(' ~> typeParsers.typeName <~ ')') >># { dest =>
        primary ^^ { IRCastExpression(dest, _) }
      }

      lazy val parenthesized: ContextSensitiveParser[IRExpression] = '(' ~> expression <~ ')'

      lazy val classLiteral: ContextFreeParser[IRClassLiteral] = objectClassLiteral | primitiveClassLiteral

      lazy val objectClassLiteral: ContextFreeParser[IRObjectClassLiteral] = typeParsers.className ~ dim_dot_class ^^ {
        case clazz ~ dim => IRObjectClassLiteral(clazz, dim)
      }

      lazy val primitiveClassLiteral: ContextFreeParser[IRPrimitiveClassLiteral] = typeParsers.primitiveTypeName ~ dim_dot_class ^^ {
        case clazz ~ dim => IRPrimitiveClassLiteral(clazz, dim)
      }

      private lazy val dim_dot_class = dimension <~ '.' <~ "class"

      lazy val thisRef: ContextFreeParser[IRThisRef] = "this" ^^? { _ => thisType.map(IRThisRef) }

      lazy val variableRef: ContextSensitiveParser[IRLocalVariableRef] = identifier >># { name => parserEnvironment ^^? { _.localVariable(name)} }

      lazy val intExpression: ContextSensitiveParser[IRExpression] = getExpressionParser(intType)

      lazy val dimension: ContextFreeParser[Int] = ('[' ~> ']').* ^^ { _.length }

      lazy val dimension1: ContextFreeParser[Int] = ('[' ~> ']').+ ^^ { _.length }

      private def procedureCall [T : HasEnvModify] (procedure: JProcedure)(f: (Map[String, MetaArgument], List[IRExpression], List[IRContextRef]) => T): ContextSensitiveParser[T] = getArgumentsParser(procedure) >> {
        case (bind, args) => parserEnvironment ^^? { _.inferContexts(procedure, bind).map(f(bind, args, _)) }
      }

      private def procedureCall [T : HasEnvModify] (procedure: JProcedure, metaArgs: List[MetaArgument])(f: (Map[String, MetaArgument], List[IRExpression], List[IRContextRef]) => T): ContextSensitiveParser[T] = {
        if (metaArgs.isEmpty) procedureCall(procedure)(f)
        else binding(procedure, metaArgs).map { bind =>
          getArgumentsParser(procedure, bind) >> { args =>
            parserEnvironment ^^? { _.inferContexts(procedure, bind) } ^^ { f(bind, args, _) }
          }
        }.getOrElse(ContextSensitiveParser.failure[T]("type mismatch for procedure " + procedure))
      }

      private def constructorCall (metaArgs: List[MetaArgument], constructor: JConstructor) = procedureCall(constructor, metaArgs) {
        case (bind, args, cs) => IRNewExpression(bind, constructor, args, cs)
      }

      private def thisConstructorCall (metaArgs: List[MetaArgument], constructor: JConstructor) = procedureCall(constructor, metaArgs) {
        case (bind, args, cs) => IRThisConstructorCall(bind, constructor, args, cs)
      }

      private def superConstructorCall (metaArgs: List[MetaArgument], constructor: JConstructor) = procedureCall(constructor, metaArgs) {
        case (bind, args, cs) => IRSuperConstructorCall(bind, constructor, args, cs)
      }

      private def invokeVirtual (instance: IRExpression, method: JMethod) = procedureCall(method) {
        case (bind, args, cs) => IRInstanceMethodCall(instance, bind, method, args, cs)
      }

      private def invokeVirtual (instance: IRExpression, metaArgs: List[MetaArgument], method: JMethod) = procedureCall(method, metaArgs) {
        case (bind, args, cs) => IRInstanceMethodCall(instance, bind, method, args, cs)
      }

      private def invokeSpecial (thisType: JObjectType, metaArgs: List[MetaArgument], method: JMethod) = procedureCall(method, metaArgs) {
        case (bind, args, cs) => IRSuperMethodCall(thisType, bind, method, args, cs)
      }

      private def invokeStatic (method: JMethod) = procedureCall(method) {
        case (bind, args, cs) => IRStaticMethodCall(bind, method, args, cs)
      }

      private def invokeStatic (metaArgs: List[MetaArgument], method: JMethod) = procedureCall(method, metaArgs) {
        case (bind, args, cs) => IRStaticMethodCall(bind, method, args, cs)
      }

      private def binding(procedure: JProcedure, metaArgs: List[MetaArgument]): Option[Map[String, MetaArgument]] = {
        val metaParams = procedure.methodDef.signature.metaParams
        if (validTypeArgs(metaParams, metaArgs, procedure.env)) Some(metaParams.map(_.name).zip(metaArgs).toMap)
        else None
      }
    }
  }
}