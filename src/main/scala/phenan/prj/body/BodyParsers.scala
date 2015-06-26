package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.exception.ParseException
import phenan.prj.ir._

import scala.language.implicitConversions
import scala.util._
import scala.util.parsing.input.CharSequenceReader

import scalaz.Memo._

class BodyParsers (compiler: JCompiler) extends TwoLevelParsers {
  type Elem = Char

  def parse [T] (parser: HParser[T], in: String): Try[T] = parser(new CharSequenceReader(in)) match {
    case ParseSuccess(result, _) => Success(result)
    case ParseFailure(msg, _)    => Failure(ParseException(msg))
  }

  class StatementParsers private (returnType: JType, env: Environment) {
    lazy val block = '{' ~> blockStatements <~ '}' ^^ IRBlock

    lazy val blockStatements: HParser[List[IRStatement]] = statement_BlockStatements | local_BlockStatements | expression_BlockStatements | success(Nil).^

    private lazy val statement_BlockStatements = statement ~ blockStatements ^^ { case s ~ bs => s :: bs }

    private lazy val local_BlockStatements = localDeclarationStatement >> { local =>
      StatementParsers(returnType, env.defineLocals(local.declaration)).blockStatements ^^ { local :: _ }
    }

    private lazy val expression_BlockStatements = expressionStatement >> { es =>
      StatementParsers(returnType, env.modifyContext(es.expression)).blockStatements ^^ { es :: _ }
    }

    lazy val statement: HParser[IRStatement] = block | controlStatement

    lazy val controlStatement: HParser[IRStatement] = ifStatement | whileStatement | forStatement | returnStatement

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


    lazy val returnStatement = "return" ~> expression(returnType) <~ ';' ^^ IRReturnStatement

    lazy val localDeclarationStatement = localDeclaration <~ ';' ^^ IRLocalDeclarationStatement

    lazy val localDeclaration: HParser[IRLocalDeclaration] = typeName >> { t =>
      ExpressionParsers(t, env).variableDeclarator.+(',') ^^ { IRLocalDeclaration(t, _) }
    }

    lazy val expressionStatement = statementExpression <~ ';' ^^ IRExpressionStatement

    lazy val statementExpressionList = statementExpression.*(',')

    lazy val statementExpression = expression(compiler.typeLoader.void)


    def collection (elemType: JType) =
      compiler.state.someOrError(elemType.boxed.flatMap(compiler.typeLoader.iterableOf).map(expression).map(_ | expression(elemType.array)),
        "cannot get type object Iterable<" + elemType.name + ">" , expression(elemType.array))


    def expression (expected: JType) = ExpressionParsers(expected, env).expression

    lazy val typeName = TypeParsers(env.resolver).typeName
  }

  class ExpressionParsers private (expected: JType, env: Environment) {
    lazy val variableDeclarator: HParser[IRVariableDeclarator] = identifier ~ emptyBrackets >> {
      case id ~ dim => ( '=' ~> ExpressionParsers(expected.array(dim), env).expression ).? ^^ { IRVariableDeclarator(id, dim, _) }
    }

    def expression: HParser[IRExpression] = env.highestPriority.map(cached).getOrElse(hostExpression)

    def expression (priority: JPriority): HParser[IRExpression] = cached(priority)

    lazy val hostExpression: HParser[IRExpression] = ???

    private val cached: JPriority => HParser[IRExpression] = mutableHashMapMemo { p =>
      env.expressionOperators(expected, p).map(ExpressionOperatorParsers(_, env).operator).reduce(_ ||| _) | env.nextPriority(p).map(cached).getOrElse(hostExpression)
    }
  }

  class ExpressionOperatorParsers private (expressionOperator: ExpressionOperator, env: Environment) {
    lazy val operator: HParser[IRExpression] = constructParser(expressionOperator.syntax.pattern, Nil) ^^ expressionOperator.semantics

    def constructParser (pattern: List[JSyntaxElement], operands: List[IRArgument]): HParser[List[IRArgument]] = pattern match {
      case (e: JOperand) :: rest          => parameter(e.parameter, operands) >> { arg => constructParser(rest, operands :+ IRNormalArgument(arg)) }
      case (e: JOptionalOperand) :: rest  => parameter(e.parameter, operands).? >> { arg => constructParser(rest, operands :+ IROptionalArgument(e, arg)) }
      case (e: JRepetition0) :: rest      => parameter(e.parameter, operands).* >> { arg => constructParser(rest, operands :+ IRVariableArguments(arg)) }
      case (e: JRepetition1) :: rest      => parameter(e.parameter, operands).+ >> { arg => constructParser(rest, operands :+ IRVariableArguments(arg)) }
      case (e: JMetaOperand) :: rest      => parameter(e.parameter, operands) >> { arg => constructParser(rest, operands :+ IRMetaArgument(e, arg))}
      case JMetaName(value) :: rest       => metaValue(value) ~> constructParser(rest, operands)
      case JOperatorName(name) :: rest    => word(name).^ ~> constructParser(rest, operands)
      case JAndPredicate(param) :: rest   => parameter(param, operands).& ~> constructParser(rest, operands)
      case JNotPredicate(param) :: rest   => parameter(param, operands).! ~> constructParser(rest, operands)
      case Nil                            => success(operands).^
    }

    def parameter (param: JParameter, operands: List[IRArgument]): HParser[IRExpression] = {
      val environment = operands.collect { case arg: IRRuntimeArgument => arg }.foldLeft(env) {
        case (e, IRNormalArgument(arg))      => e.modifyContext(arg)
        case (e, IROptionalArgument(_, arg)) => arg.map(e.modifyContext).getOrElse(e)
        case (e, IRVariableArguments(args))  => args.foldLeft(e) { _.modifyContext(_) }
      }
      val bindings = operands.collect { case arg: IRMetaArgument => arg.binding }.flatMap {
        case Success(bind) => Some(bind)
        case Failure(e)    => compiler.state.errorAndReturn("invalid meta argument", e, None)
      }.toMap
      val unbound = param.genericType.unbound(bindings)

      ???
      // ExpressionParsers(param.genericType.bind(operands.collect { case arg: IRMetaArgument => arg.binding }), env.modifyContext())
    }

    def metaValue (mv: MetaValue): HParser[MetaValue] = ???
  }

  class TypeParsers private (resolver: NameResolver) {
    lazy val metaValue: HParser[MetaValue] = wildcard | metaVariable | refType
    lazy val typeName: HParser[JType] = primitiveTypeName | refType
    lazy val refType: HParser[JRefType] = arrayType | typeVariable | objectType
    lazy val objectType: HParser[JObjectType] = className ~ ( '<' ~> metaValue.+(',') <~ '>' ).? ^^? {
      case clazz ~ args => clazz.objectType(args.getOrElse(Nil))
    }
    lazy val packageName: HParser[List[String]] = (identifier <~ '.').*! { names =>
      ! resolver.root.isKnownPackage(names) && resolver.resolve(names).isSuccess
    }
    lazy val className: HParser[JClass] = ref(innerClassName | topLevelClassName)
    lazy val topLevelClassName: HParser[JClass] = packageName ~ identifier ^^? {
      case pack ~ name => resolver.resolve(pack :+ name).toOption
    }
    lazy val innerClassName: HParser[JClass] = className ~ ('.' ~> identifier) ^^? {
      case name ~ id => name.innerClasses.get(id).flatMap(compiler.classLoader.loadClass_PE)
    }
    lazy val typeVariable: HParser[JTypeVariable] = identifier ^^? resolver.typeVariable
    lazy val metaVariable: HParser[PureVariableRef] = identifier ^^? resolver.metaVariable
    lazy val arrayType: HParser[JArrayType] = typeName <~ emptyBracket ^^ { _.array }
    lazy val primitiveTypeName: HParser[JPrimitiveType] = identifier ^? {
      case "byte"    => compiler.classLoader.byte.primitiveType
      case "char"    => compiler.classLoader.char.primitiveType
      case "double"  => compiler.classLoader.double.primitiveType
      case "float"   => compiler.classLoader.float.primitiveType
      case "int"     => compiler.classLoader.int.primitiveType
      case "long"    => compiler.classLoader.long.primitiveType
      case "short"   => compiler.classLoader.short.primitiveType
      case "boolean" => compiler.classLoader.boolean.primitiveType
      case "void"    => compiler.classLoader.void.primitiveType
    }
    lazy val wildcard: HParser[JWildcard] = '?' ~> ( "extends" ~> refType ).? ~ ( "super" ~> refType ).? ^^ {
      case ub ~ lb => JWildcard(ub, lb)
    }
  }

  object StatementParsers {
    def apply (expected: JType, env: Environment): StatementParsers = cached((expected, env))
    private val cached : ((JType, Environment)) => StatementParsers = mutableHashMapMemo { pair => new StatementParsers(pair._1, pair._2) }
  }

  object ExpressionParsers {
    def apply (expected: JType, env: Environment): ExpressionParsers = cached((expected, env))
    private val cached : ((JType, Environment)) => ExpressionParsers = mutableHashMapMemo { pair => new ExpressionParsers(pair._1, pair._2) }
  }

  object ExpressionOperatorParsers {
    def apply (expressionOperator: ExpressionOperator, env: Environment): ExpressionOperatorParsers = cached((expressionOperator, env))
    private val cached : ((ExpressionOperator, Environment)) => ExpressionOperatorParsers = mutableHashMapMemo { pair => new ExpressionOperatorParsers(pair._1, pair._2) }
  }

  object TypeParsers {
    def apply (resolver: NameResolver): TypeParsers = cached(resolver)
    private val cached : NameResolver => TypeParsers = mutableHashMapMemo(new TypeParsers(_))
  }

  lazy val delimiter: LParser[Any] = elem("white space", Character.isWhitespace).*

  lazy val emptyBrackets = emptyBracket.* ^^ { _.size }
  lazy val emptyBracket = '[' ~> ']'

  lazy val qualifiedName = identifier.+('.')

  lazy val identifier = (elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).*).^ ^^ {
    case s ~ ps => (s :: ps).mkString
  }

  private def word (cs: String): LParser[String] = word_cached(cs)

  private implicit def keyword (kw: String): HParser[String] = (word(kw) <~ elem("identifier part", Character.isJavaIdentifierPart).!).^
  private implicit def symbol (ch: Char): HParser[Char] = elem(ch).^

  private lazy val word_cached: String => LParser[String] = mutableHashMapMemo { cs => cs.foldRight(success(cs)) { (ch, r) => elem(ch) ~> r } }
}
