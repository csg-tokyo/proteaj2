package phenan.prj.body

import phenan.prj._
import phenan.prj.exception.ParseException
import phenan.prj.ir._

import scala.language.implicitConversions
import scala.util._
import scala.util.parsing.input._

import scalaz.Memo._

class BodyParsers (val compiler: JCompiler)
  extends ExpressionParsers with ExpressionOperatorParsers with ExpressionOperandParsers with JavaExpressionParsers with ArgumentParsers
  with LiteralParsers with LiteralOperatorParsers with LiteralOperandParsers with JavaLiteralParsers with TypeParsers with CommonParsers
{
  def parse [T] (parser: HParser[T], in: String): Try[T] = parser(new CharSequenceReader(in)) match {
    case ParseSuccess(result, _) => Success(result)
    case ParseFailure(msg, _)    => Failure(ParseException(msg))
  }

  class StatementParsers private (returnType: JType, env: Environment) {
    lazy val methodBody = block ^^ IRMethodBody

    lazy val constructorBody = '{' ~> ( getExplicitConstructorCallParser(env).? ~ blockStatements ) <~ '}' ^^ {
      case ecc ~ statements => IRConstructorBody(ecc, statements)
    }

    lazy val initializerBody = block ^^ IRInitializerBody

    lazy val block = '{' ~> blockStatements <~ '}' ^^ IRBlock

    lazy val blockStatements: HParser[List[IRStatement]] = blockStatement >> {
      case s @ IRLocalDeclarationStatement(local) => StatementParsers(returnType, env.defineLocals(local)).blockStatements ^^ { s :: _ }
      case s : IRExpressionStatement              => StatementParsers(returnType, env.modifyContext(s)).blockStatements ^^ { s :: _ }
      case s => blockStatements ^^ { s :: _ }
    } | HParser.success(Nil)

    lazy val blockStatement: HParser[IRStatement] = localDeclarationStatement | statement

    lazy val statement: HParser[IRStatement] = block | controlStatement | expressionStatement

    lazy val controlStatement: HParser[IRStatement] = ifStatement | whileStatement | forStatement | activateStatement | throwStatement | returnStatement

    lazy val ifStatement: HParser[IRIfStatement] = ( "if" ~> '(' ~> expression(compiler.typeLoader.boolean) <~ ')' ) ~ statement ~ ( "else" ~> statement ).? ^^ {
      case cond ~ thenStmt ~ elseStmt => IRIfStatement(cond, thenStmt, elseStmt)
    }

    lazy val whileStatement = ( "while" ~> '(' ~> expression(compiler.typeLoader.boolean) <~ ')' ) ~ statement ^^ {
      case cond ~ stmt => IRWhileStatement(cond, stmt)
    }

    lazy val forStatement = normalForStatement | ancientForStatement | enhancedForStatement

    lazy val normalForStatement = "for" ~> '(' ~> localDeclaration <~ ';' >> { local =>
      StatementParsers(returnType, env.defineLocals(local)).forControlRest ^^ {
        case cond ~ update ~ stmt => IRNormalForStatement(local, cond, update, stmt)
      }
    }

    lazy val ancientForStatement = "for" ~> '(' ~> ( statementExpressionList <~ ';' ) ~ forControlRest ^^ {
      case init ~ ( cond ~ update ~ stmt ) => IRAncientForStatement(init, cond, update, stmt)
    }

    lazy val enhancedForStatement = ( "for" ~> '(' ~> typeName ) ~ identifier ~ emptyBrackets <~ ':' >> {
      case elemType ~ name ~ dim => collection(elemType.array(dim)) ~ ( ')' ~> StatementParsers(returnType, env.defineLocal(elemType.array(dim), name)).statement ) ^^ {
        case set ~ stmt => IREnhancedForStatement(elemType, name, dim, set, stmt)
      }
    }

    private lazy val forControlRest = expression(compiler.typeLoader.boolean).? ~ ( ';' ~> statementExpressionList ) ~ ( ')' ~> statement )

    lazy val activateStatement = "activate" ~> env.activateTypes.map(expression).reduceOption(_ | _).getOrElse(HParser.failure("activates clause is not found")) <~ ';' ^^ IRActivateStatement

    lazy val throwStatement = "throw" ~> env.exceptions.map(expression).reduceOption(_ | _).getOrElse(HParser.failure("exception types are not found")) <~ ';' ^^ IRThrowStatement

    lazy val returnStatement = "return" ~> expression(returnType) <~ ';' ^^ IRReturnStatement

    lazy val localDeclarationStatement = localDeclaration <~ ';' ^^ IRLocalDeclarationStatement

    lazy val localDeclaration: HParser[IRLocalDeclaration] = typeName >> { t =>
      variableDeclarator(t).+(',') ^^ { IRLocalDeclaration(t, _) }
    }

    def variableDeclarator(expected: JType): HParser[IRVariableDeclarator] = identifier ~ emptyBrackets >> {
      case id ~ dim => ( '=' ~> expression(expected.array(dim)) ).? ^^ { IRVariableDeclarator(id, dim, _) }
    }

    lazy val expressionStatement = statementExpression <~ ';' ^^ IRExpressionStatement

    lazy val statementExpressionList = statementExpression.*(',')

    lazy val statementExpression = expression(compiler.typeLoader.void)


    def collection (elemType: JType) =
      compiler.state.someOrError(elemType.boxed.flatMap(compiler.typeLoader.iterableOf).map(expression).map(_ | expression(elemType.array)),
        "cannot get type object Iterable<" + elemType.name + ">" , expression(elemType.array))


    def expression (expected: JType) = getExpressionParser(expected, env)

    lazy val typeName = getTypeParsers(env.resolver).typeName
  }

  object StatementParsers {
    def apply (expected: JType, env: Environment): StatementParsers = cached((expected, env))
    private val cached : ((JType, Environment)) => StatementParsers = mutableHashMapMemo { pair => new StatementParsers(pair._1, pair._2) }
  }
}
