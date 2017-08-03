package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scalaz.Memo._

trait StatementParsersModule {
  this: ExpressionParsersModule with JavaExpressionParsersModule with TypeParsersModule
    with CommonParsersModule with ContextSensitiveParsersModule
    with JTypeLoader with Environments with EnvModifyStrategy
    with IRStatements with IRExpressions with JModules with Application =>

  trait StatementParsers {
    this: ExpressionParsers with JavaExpressionParsers with TypeParsers with CommonParsers with ContextSensitiveParsers =>

    def getStatementParsers (returnType: JType): StatementParsersInterface = cached(returnType)

    trait StatementParsersInterface {
      def methodBody: ContextSensitiveParser[IRMethodBody]

      def constructorBody: ContextSensitiveParser[IRConstructorBody]

      def initializerBody: ContextSensitiveParser[IRInitializerBody]

      def block: ContextSensitiveParser[IRBlock]

      def statement: ContextSensitiveParser[IRStatement]
    }

    private val cached: JType => StatementParsersInterface = mutableHashMapMemo { new StatementParsersImpl(_) }

    private class StatementParsersImpl(returnType: JType) extends StatementParsersInterface {
      lazy val methodBody: ContextSensitiveParser[IRMethodBody] = block ^^ IRMethodBody

      lazy val constructorBody: ContextSensitiveParser[IRConstructorBody] = '{' ~> ((explicitConstructorCallParser <~ ';').? ~ blockStatement.*) <~ '}' ^^ {
        case ecc ~ statements => IRConstructorBody(ecc, statements)
      }

      lazy val initializerBody: ContextSensitiveParser[IRInitializerBody] = block ^^ IRInitializerBody

      lazy val block: ContextSensitiveParser[IRBlock] = '{' ~> blockStatement.* <~ '}' ^^ IRBlock

      lazy val blockStatement: ContextSensitiveParser[IRStatement] = localDeclarationStatement | statement

      lazy val statement: ContextSensitiveParser[IRStatement] = block | controlStatement | expressionStatement

      lazy val controlStatement: ContextSensitiveParser[IRStatement] = ifStatement | whileStatement | forStatement | tryStatement | activateStatement | throwStatement | returnStatement

      lazy val ifStatement: ContextSensitiveParser[IRIfStatement] = ("if" ~> '(' ~> expression(booleanType) <~ ')') ~ statement ~ ("else" ~> statement).? ^^ {
        case cond ~ thenStmt ~ elseStmt => IRIfStatement(cond, thenStmt, elseStmt)
      }

      lazy val whileStatement: ContextSensitiveParser[IRWhileStatement] = ("while" ~> '(' ~> expression(booleanType) <~ ')') ~ statement ^^ {
        case cond ~ stmt => IRWhileStatement(cond, stmt)
      }

      lazy val forStatement: ContextSensitiveParser[IRForStatement with Product with Serializable] = normalForStatement | ancientForStatement | enhancedForStatement

      lazy val normalForStatement: ContextSensitiveParser[IRNormalForStatement] = ( "for" ~> '(' ~> localDeclaration <~ ';' ) ~ forControlRest ^^ {
        case local ~ (cond ~ update ~ stmt) => IRNormalForStatement(local, cond, update, stmt)
      }

      lazy val ancientForStatement: ContextSensitiveParser[IRAncientForStatement] = "for" ~> '(' ~> (statementExpressionList <~ ';') ~ forControlRest ^^ {
        case init ~ (cond ~ update ~ stmt) => IRAncientForStatement(init, cond, update, stmt)
      }

      lazy val enhancedForStatement: ContextSensitiveParser[IREnhancedForStatement] = enhancedForHeader ~ getStatementParsers(returnType).statement ^^ {
        case header ~ body => IREnhancedForStatement(header, body)
      }

      lazy val enhancedForHeader: ContextSensitiveParser[IREnhancedForHeader] = ("for" ~> '(' ~> typeName) ~ identifier ~ emptyBrackets <~ ':' >># {
        case elemType ~ name ~ dim => collection(elemType.array(dim)) <~ ')' ^^ { IREnhancedForHeader(elemType, name, dim, _) }
      }

      lazy val forControlRest: ContextSensitiveParser[Option[IRExpression] ~ List[IRExpression] ~ IRStatement] = expression(booleanType).? ~ (';' ~> statementExpressionList) ~ (')' ~> statement)

      lazy val tryStatement: ContextSensitiveParser[IRTryStatement] = tryFinallyStatement | tryCatchStatement

      lazy val tryFinallyStatement: ContextSensitiveParser[IRTryStatement] = ("try" ~> block) ~ ("finally" ~> block) ^^ { case tryBlock ~ finallyBlock => IRTryStatement(tryBlock, Nil, Some(finallyBlock)) }

      lazy val tryCatchStatement: ContextSensitiveParser[IRTryStatement] = ("try" ~> block) ~ exceptionHandler.+ ~ ("finally" ~> block).? ^^ {
        case tryBlock ~ catchBlocks ~ finallyBlock => IRTryStatement(tryBlock, catchBlocks, finallyBlock)
      }

      private lazy val exceptionHandler: ContextSensitiveParser[IRExceptionHandler] = exceptionHandlerHeader ~ getStatementParsers(returnType).block ^^ {
        case header ~ body => IRExceptionHandler(header, body)
      }

      private lazy val exceptionHandlerHeader: ContextSensitiveParser[IRExceptionHandlerHeader] = ("catch" ~> '(' ~> typeName) ~ (identifier <~ ')') ^^# {
        case t ~ id => IRExceptionHandlerHeader(t, id)
      }

      lazy val activateStatement: ContextSensitiveParser[IRActivateStatement] = "activate" ~> ContextSensitiveParser { env =>
        val ps = env.activateTypes.map(a => expression(a) <~ ';' ^^ IRActivateStatement)
        ps.foldRight(ContextSensitiveParser.failure[IRActivateStatement]("activates clause is not found"))(_ | _)
      }

      lazy val throwStatement: ContextSensitiveParser[IRThrowStatement] = "throw" ~> ContextSensitiveParser { env =>
        val ps = env.exceptions.map(e => expression(e) <~ ';' ^^ IRThrowStatement)
        ps.foldRight(ContextSensitiveParser.failure[IRThrowStatement]("exception types are not found"))(_ | _)
      }

      lazy val returnStatement: ContextSensitiveParser[IRReturnStatement] = "return" ~> expression(returnType) <~ ';' ^^ IRReturnStatement

      lazy val localDeclarationStatement: ContextSensitiveParser[IRLocalDeclarationStatement] = localDeclaration <~ ';' ^^ { local =>
        IRLocalDeclarationStatement(local)
      }

      lazy val localDeclaration: ContextSensitiveParser[IRLocalDeclaration] = typeName >># { t =>
        variableDeclarator(t).+(',') ^^ {
          IRLocalDeclaration(t, _)
        }
      }

      def variableDeclarator(expected: JType): ContextSensitiveParser[IRVariableDeclarator] = identifier ~ emptyBrackets >># {
        case id ~ dim => ('=' ~> expression(expected.array(dim))).? ^^ {
          IRVariableDeclarator(id, dim, _)
        }
      }

      lazy val expressionStatement: ContextSensitiveParser[IRExpressionStatement] = statementExpression <~ ';' ^^ IRExpressionStatement

      lazy val statementExpressionList: ContextSensitiveParser[List[IRExpression]] = statementExpression.*(',')

      lazy val statementExpression: ContextSensitiveParser[IRExpression] = expression(voidType)


      def collection(elemType: JType): ContextSensitiveParser[IRExpression] = expression(iterableOf(elemType)) | expression(elemType.array)

      def expression(expected: JType): ContextSensitiveParser[IRExpression] = getExpressionParser(expected)

      lazy val typeName: ContextFreeParser[JType] = typeParsers.typeName
    }

  }
}
