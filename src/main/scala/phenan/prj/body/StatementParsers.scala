package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scalaz.Memo._

trait StatementParsers {
  this: ExpressionParsers with JavaExpressionParsers with TypeParsers with CommonParsers with TwoLevelParsers =>

  def getStatementParsers (returnType: JType, env: Environment): StatementParsersInterface = cached((returnType, env))

  trait StatementParsersInterface {
    def methodBody: HParser[IRMethodBody]
    def constructorBody: HParser[IRConstructorBody]
    def initializerBody: HParser[IRInitializerBody]
    def block: HParser[IRBlock]
    def blockStatements: HParser[List[IRStatement]]
    def statement: HParser[IRStatement]
    protected[StatementParsers] def forControlRest: HParser[Option[IRExpression] ~ List[IRExpression] ~ IRStatement]
  }

  private val cached: ((JType, Environment)) => StatementParsersInterface = mutableHashMapMemo { pair =>
    new StatementParsersImpl(pair._1, pair._2)
  }

  private class StatementParsersImpl (returnType: JType, env: Environment) extends StatementParsersInterface {
    lazy val methodBody: HParser[IRMethodBody] = block ^^ IRMethodBody

    lazy val constructorBody: HParser[IRConstructorBody] = '{' ~> ( getExplicitConstructorCallParser(env).? ~ blockStatements ) <~ '}' ^^ {
      case ecc ~ statements => IRConstructorBody(ecc, statements)
    }

    lazy val initializerBody: HParser[IRInitializerBody] = block ^^ IRInitializerBody

    lazy val block: HParser[IRBlock] = '{' ~> blockStatements <~ '}' ^^ IRBlock

    lazy val blockStatements: HParser[List[IRStatement]] = blockStatement >> {
      case s @ IRLocalDeclarationStatement(local) => getStatementParsers(returnType, env.defineLocals(local)).blockStatements ^^ { s :: _ }
      case s : IRExpressionStatement              => getStatementParsers(returnType, env.modifyContext(s)).blockStatements ^^ { s :: _ }
      case s => blockStatements ^^ { s :: _ }
    } | HParser.success(Nil)

    lazy val blockStatement: HParser[IRStatement] = localDeclarationStatement | statement

    lazy val statement: HParser[IRStatement] = block | controlStatement | expressionStatement

    lazy val controlStatement: HParser[IRStatement] = ifStatement | whileStatement | forStatement | tryStatement | activateStatement | throwStatement | returnStatement

    lazy val ifStatement: HParser[IRIfStatement] = ( "if" ~> '(' ~> expression(compiler.typeLoader.boolean) <~ ')' ) ~ statement ~ ( "else" ~> statement ).? ^^ {
      case cond ~ thenStmt ~ elseStmt => IRIfStatement(cond, thenStmt, elseStmt)
    }

    lazy val whileStatement: HParser[IRWhileStatement] = ( "while" ~> '(' ~> expression(compiler.typeLoader.boolean) <~ ')' ) ~ statement ^^ {
      case cond ~ stmt => IRWhileStatement(cond, stmt)
    }

    lazy val forStatement: HParser[IRForStatement with Product with Serializable] = normalForStatement | ancientForStatement | enhancedForStatement

    lazy val normalForStatement: HParser[IRNormalForStatement] = "for" ~> '(' ~> localDeclaration <~ ';' >> { local =>
      getStatementParsers(returnType, env.defineLocals(local)).forControlRest ^^ {
        case cond ~ update ~ stmt => IRNormalForStatement(local, cond, update, stmt)
      }
    }

    lazy val ancientForStatement: HParser[IRAncientForStatement] = "for" ~> '(' ~> ( statementExpressionList <~ ';' ) ~ forControlRest ^^ {
      case init ~ ( cond ~ update ~ stmt ) => IRAncientForStatement(init, cond, update, stmt)
    }

    lazy val enhancedForStatement: HParser[IREnhancedForStatement] = ( "for" ~> '(' ~> typeName ) ~ identifier ~ emptyBrackets <~ ':' >> {
      case elemType ~ name ~ dim => collection(elemType.array(dim)) ~ ( ')' ~> getStatementParsers(returnType, env.defineLocal(elemType.array(dim), name)).statement ) ^^ {
        case set ~ stmt => IREnhancedForStatement(elemType, name, dim, set, stmt)
      }
    }

    protected[StatementParsers] lazy val forControlRest: HParser[Option[IRExpression] ~ List[IRExpression] ~ IRStatement] = expression(compiler.typeLoader.boolean).? ~ ( ';' ~> statementExpressionList ) ~ ( ')' ~> statement )

    lazy val tryStatement: HParser[IRTryStatement] = tryFinallyStatement | tryCatchStatement

    lazy val tryFinallyStatement: HParser[IRTryStatement] = ( "try" ~> block ) ~ ( "finally" ~> block ) ^^ { case tryBlock ~ finallyBlock => IRTryStatement(tryBlock, Nil, Some(finallyBlock)) }

    lazy val tryCatchStatement: HParser[IRTryStatement] = ( "try" ~> block ) ~ exceptionHandler.+ ~ ( "finally" ~> block ).? ^^ {
      case tryBlock ~ catchBlocks ~ finallyBlock => IRTryStatement(tryBlock, catchBlocks, finallyBlock)
    }

    lazy val exceptionHandler: HParser[IRExceptionHandler] = ( "catch" ~> '(' ~> typeName ) ~ ( identifier <~ ')' ) >> {
      case t ~ id => getStatementParsers(returnType, env.defineLocal(t, id)).block ^^ { IRExceptionHandler(t, id, _) }
    }

    lazy val activateStatement: HParser[IRActivateStatement] = "activate" ~> env.activateTypes.map(expression).reduceOption(_ | _).getOrElse(HParser.failure("activates clause is not found")) <~ ';' ^^ IRActivateStatement

    lazy val throwStatement: HParser[IRThrowStatement] = "throw" ~> env.exceptions.map(expression).reduceOption(_ | _).getOrElse(HParser.failure("exception types are not found")) <~ ';' ^^ IRThrowStatement

    lazy val returnStatement: HParser[IRReturnStatement] = "return" ~> expression(returnType) <~ ';' ^^ IRReturnStatement

    lazy val localDeclarationStatement: HParser[IRLocalDeclarationStatement] = localDeclaration <~ ';' ^^ IRLocalDeclarationStatement

    lazy val localDeclaration: HParser[IRLocalDeclaration] = typeName >> { t =>
      variableDeclarator(t).+(',') ^^ { IRLocalDeclaration(t, _) }
    }

    def variableDeclarator(expected: JType): HParser[IRVariableDeclarator] = identifier ~ emptyBrackets >> {
      case id ~ dim => ( '=' ~> expression(expected.array(dim)) ).? ^^ { IRVariableDeclarator(id, dim, _) }
    }

    lazy val expressionStatement: HParser[IRExpressionStatement] = statementExpression <~ ';' ^^ IRExpressionStatement

    lazy val statementExpressionList: HParser[List[IRExpression]] = statementExpression.*(',')

    lazy val statementExpression: HParser[IRExpression] = expression(compiler.typeLoader.void)


    def collection (elemType: JType): HParser[IRExpression] =
      compiler.state.someOrError(elemType.boxed.flatMap(compiler.typeLoader.iterableOf).map(expression).map(_ | expression(elemType.array)),
        "cannot get type object Iterable<" + elemType.name + ">" , expression(elemType.array))


    def expression (expected: JType): HParser[IRExpression] = getExpressionParser(expected, env)

    lazy val typeName: HParser[JType] = getTypeParsers(env.resolver).typeName
  }
}
