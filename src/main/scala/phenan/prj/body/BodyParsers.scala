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

    def expression: HParser[IRExpression] = env.highestPriority.map(expression_cached).getOrElse(hostExpression)

    def expression (priority: JPriority): HParser[IRExpression] = expression_cached(priority)

    def expression (priority: Option[JPriority]): HParser[IRExpression] = priority match {
      case Some(p) => expression(p)
      case None    => hostExpression
    }

    def literal: LParser[IRExpression] = env.highestPriority.map(literal_cached).getOrElse(hostLiteral)

    def literal (priority: JPriority): LParser[IRExpression] = literal_cached(priority)

    def literal (priority: Option[JPriority]): LParser[IRExpression] = priority match {
      case Some(p) => literal(p)
      case None    => hostLiteral
    }

    lazy val parenthesized: HParser[IRExpression] = '(' ~> expression <~ ')'

    lazy val hostExpression: HParser[IRExpression] = ???

    lazy val hostLiteral: LParser[IRExpression] = ???

    private val expression_cached: JPriority => HParser[IRExpression] = mutableHashMapMemo { p =>
      env.expressionOperators(expected, p).map(ExpressionOperatorParsers(_, env).operator).reduce(_ ||| _) | env.nextPriority(p).map(expression_cached).getOrElse(hostExpression)
    }

    private val literal_cached: JPriority => LParser[IRExpression] = mutableHashMapMemo { p =>
      env.literalOperators(expected, p).map(LiteralOperatorParsers(_, env).operator).reduce(_ ||| _) | env.nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
    }
  }

  object JavaExpressionParsers {

  }

  class ExpressionOperatorParsers private (eop: ExpressionOperator, env: Environment) {
    lazy val operator: HParser[IRExpression] = constructParser(eop.syntax.pattern, eop.metaArgs, env, Nil)

    private def constructParser (pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], environment: Environment, operands: List[IRExpression]): HParser[IRExpression] = pattern match {
      case JOperand(param) :: rest           => parameter(param, binding, environment) >> { arg =>
        constructParser(rest, bind(param, arg, binding), environment.modifyContext(arg), arg :: operands)
      }
      case JOptionalOperand(param) :: rest   => optional(param, binding, environment) >> {
        case Some(arg) => constructParser(rest, bind(param, arg, binding), environment.modifyContext(arg), arg :: operands)
        case None      => constructParser(rest, binding, environment, operands)
      }
      case JRepetition0(param) :: rest       => rep0(param, binding, environment, Nil) >> {
        case (bnd, e, arg) => constructParser(rest, bnd, e, arg :: operands)
      }
      case JRepetition1(param) :: rest       => rep1(param, binding, environment) >> {
        case (bnd, e, arg) => constructParser(rest, bnd, e, arg :: operands)
      }
      case JMetaOperand(name, param) :: rest => parameter(param, binding, environment) >> {
        ast => constructParser(rest, binding + (name -> ConcretePureValue(ast, param)), environment, operands)
      }
      case JMetaName(value) :: rest          => metaValue(value, binding, environment) ~> constructParser(rest, binding, environment, operands)
      case JOperatorName(name) :: rest       => word(name).^ ~> constructParser(rest, binding, environment, operands)
      case JAndPredicate(param) :: rest      => parameter(param, binding, environment).& ~> constructParser(rest, binding, environment, operands)
      case JNotPredicate(param) :: rest      => parameter(param, binding, environment).! ~> constructParser(rest, binding, environment, operands)
      case Nil                               => success(eop.semantics(binding, operands.reverse)).^
    }

    private def parameter (param: JParameter, binding: Map[String, MetaArgument], environment: Environment): HParser[IRExpression] = {
      val expected = param.genericType.bind(binding ++ param.genericType.unbound(binding).flatMap(name => eop.method.metaParameters.get(name).map(name -> _)).toMap.mapValues {
        case FormalMetaParameter(name, JTypeSignature.typeTypeSig, _, bounds) => JUnboundTypeVariable(name, bounds.flatMap(compiler.typeLoader.fromTypeSignature_RefType(_, binding)), compiler)
        case FormalMetaParameter(name, metaType, _, _) => ???
      }).getOrElse {
        compiler.state.error("expected type cannot be known")
        compiler.typeLoader.void
      }
      val priority = param.priority.orElse(environment.nextPriority(eop.syntax.priority))
      ExpressionParsers(expected, environment).expression(priority)
    }

    private def metaValue (mv: MetaArgument, binding: Map[String, MetaArgument], environment: Environment): HParser[MetaArgument] = mv match {
      case t: JRefType  => TypeParsers(env.resolver).refType ^? { case v if t == v => t }
      case w: JWildcard => TypeParsers(env.resolver).wildcard ^? { case v if w == v => w }
      case r: MetaVariableRef   => TypeParsers(env.resolver).metaVariable ^? { case v if r == v => r }
      case c: ConcretePureValue => parameter(c.parameter, binding, environment) ^? { case v if c.ast == v => c }
    }

    private def optional (param: JParameter, binding: Map[String, MetaArgument], environment: Environment) = parameter(param, binding, environment).? ^^ { _.orElse(defaultExpression(param)) }

    private def rep0 (param: JParameter, binding: Map[String, MetaArgument], environment: Environment, result: List[IRExpression]): HParser[(Map[String, MetaArgument], Environment, IRExpression)] = {
      parameter(param, binding, environment) >> { arg =>
        rep0(param, bind(param, arg, binding), environment.modifyContext(arg), arg :: result)
      } | success(binding, environment, IRVariableArguments(result.reverse)).^
    }

    private def rep1 (param: JParameter, binding: Map[String, MetaArgument], environment: Environment): HParser[(Map[String, MetaArgument], Environment, IRExpression)] = {
      parameter(param, binding, environment) >> { arg => rep0(param, bind(param, arg, binding), environment.modifyContext(arg), List(arg)) }
    }

    private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = binding ++ arg.staticType.flatMap(compiler.unifier.infer(_, param.genericType)).getOrElse(Map.empty)

    private def defaultExpression (param: JParameter) = param.defaultArg.flatMap(eop.method.clazz.classModule.methods.get).flatMap(_.find(_.erasedParameterTypes == Nil)).map(IRStaticMethodCall(_, Map.empty, Nil))
  }

  class LiteralOperatorParsers private (lop: LiteralOperator, env: Environment) {
    lazy val operator: LParser[IRExpression] = constructParser(lop.syntax.pattern, lop.metaArgs, env, Nil)

    private def constructParser (pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], environment: Environment, operands: List[IRExpression]): LParser[IRExpression] = pattern match {
      case JOperand(param) :: rest           => parameter(param, binding, environment) >> { arg =>
        constructParser(rest, bind(param, arg, binding), environment.modifyContext(arg), arg :: operands)
      }
      case JOptionalOperand(param) :: rest   => optional(param, binding, environment) >> {
        case Some(arg) => constructParser(rest, bind(param, arg, binding), environment.modifyContext(arg), arg :: operands)
        case None      => constructParser(rest, binding, environment, operands)
      }
      case JRepetition0(param) :: rest       => rep0(param, binding, environment, Nil) >> {
        case (bnd, e, arg) => constructParser(rest, bnd, e, arg :: operands)
      }
      case JRepetition1(param) :: rest       => rep1(param, binding, environment) >> {
        case (bnd, e, arg) => constructParser(rest, bnd, e, arg :: operands)
      }
      case JMetaOperand(name, param) :: rest => parameter(param, binding, environment) >> {
        ast => constructParser(rest, binding + (name -> ConcretePureValue(ast, param)), environment, operands)
      }
      case JMetaName(value) :: rest          => metaValue(value, binding, environment) ~> constructParser(rest, binding, environment, operands)
      case JOperatorName(name) :: rest       => word(name) ~> constructParser(rest, binding, environment, operands)
      case JAndPredicate(param) :: rest      => parameter(param, binding, environment).& ~> constructParser(rest, binding, environment, operands)
      case JNotPredicate(param) :: rest      => parameter(param, binding, environment).! ~> constructParser(rest, binding, environment, operands)
      case Nil                               => success(lop.semantics(binding, operands.reverse))
    }

    private def parameter (param: JParameter, binding: Map[String, MetaArgument], environment: Environment): LParser[IRExpression] = {
      val expected = param.genericType.bind(binding ++ param.genericType.unbound(binding).flatMap(name => lop.method.metaParameters.get(name).map(name -> _)).toMap.mapValues {
        case FormalMetaParameter(name, JTypeSignature.typeTypeSig, _, bounds) => JUnboundTypeVariable(name, bounds.flatMap(compiler.typeLoader.fromTypeSignature_RefType(_, binding)), compiler)
        case FormalMetaParameter(name, metaType, _, _) => ???
      }).getOrElse {
        compiler.state.error("expected type cannot be known")
        compiler.typeLoader.void
      }
      val priority = param.priority.orElse(environment.nextPriority(lop.syntax.priority))
      ExpressionParsers(expected, environment).literal(priority)
    }

    private def metaValue (mv: MetaArgument, binding: Map[String, MetaArgument], environment: Environment): LParser[MetaArgument] = mv match {
      case c: ConcretePureValue => parameter(c.parameter, binding, environment) ^? { case v if c.ast == v => c }
      case _: JRefType | _: JWildcard | _: MetaVariableRef => failure("type name cannot be used in a literal")
    }

    private def optional (param: JParameter, binding: Map[String, MetaArgument], environment: Environment) = parameter(param, binding, environment).? ^^ { _.orElse(defaultExpression(param)) }

    private def rep0 (param: JParameter, binding: Map[String, MetaArgument], environment: Environment, result: List[IRExpression]): LParser[(Map[String, MetaArgument], Environment, IRExpression)] = {
      parameter(param, binding, environment) >> { arg =>
        rep0(param, bind(param, arg, binding), environment.modifyContext(arg), arg :: result)
      } | success(binding, environment, IRVariableArguments(result.reverse))
    }

    private def rep1 (param: JParameter, binding: Map[String, MetaArgument], environment: Environment): LParser[(Map[String, MetaArgument], Environment, IRExpression)] = {
      parameter(param, binding, environment) >> { arg => rep0(param, bind(param, arg, binding), environment.modifyContext(arg), List(arg)) }
    }

    private def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = binding ++ arg.staticType.flatMap(compiler.unifier.infer(_, param.genericType)).getOrElse(Map.empty)

    private def defaultExpression (param: JParameter) = param.defaultArg.flatMap(lop.method.clazz.classModule.methods.get).flatMap(_.find(_.erasedParameterTypes == Nil)).map(IRStaticMethodCall(_, Map.empty, Nil))
  }

  class TypeParsers private (resolver: NameResolver) {
    lazy val metaValue: HParser[MetaArgument] = wildcard | metaVariable | refType
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
    lazy val metaVariable: HParser[MetaVariableRef] = identifier ^^? resolver.metaVariable
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

  object LiteralOperatorParsers {
    def apply (literalOperator: LiteralOperator, env: Environment): LiteralOperatorParsers = cached((literalOperator, env))
    private val cached : ((LiteralOperator, Environment)) => LiteralOperatorParsers = mutableHashMapMemo { pair => new LiteralOperatorParsers(pair._1, pair._2) }
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
