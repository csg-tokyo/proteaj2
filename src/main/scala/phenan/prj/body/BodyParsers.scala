package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.exception.ParseException
import phenan.prj.ir._

import scala.language.implicitConversions
import scala.util._
import scala.util.parsing.input._
import CharArrayReader.EofCh

import scalaz.Memo._

class BodyParsers (compiler: JCompiler) extends TwoLevelParsers {
  type Elem = Char

  def parse [T] (parser: HParser[T], in: String): Try[T] = parser(new CharSequenceReader(in)) match {
    case ParseSuccess(result, _) => Success(result)
    case ParseFailure(msg, _)    => Failure(ParseException(msg))
  }

  class StatementParsers private (returnType: JType, env: Environment) {
    lazy val methodBody = block ^^ IRMethodBody

    lazy val constructorBody = '{' ~> ( JavaExpressionParsers(env).explicitConstructorCall.? ~ blockStatements ) <~ '}' ^^ {
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

    lazy val controlStatement: HParser[IRStatement] = ifStatement | whileStatement | forStatement | activateStatement | returnStatement

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

    lazy val hostExpression: HParser[IRExpression] = javaExpression | parenthesized | hostLiteral.^

    lazy val javaExpression = JavaExpressionParsers(env).expression ^? { case e if e.staticType.exists(_ <:< expected) || expected == compiler.typeLoader.void => e }

    lazy val hostLiteral: LParser[IRExpression] = JavaLiteralParsers.literal(expected)

    private val expression_cached: JPriority => HParser[IRExpression] = mutableHashMapMemo { p =>
      env.expressionOperators(expected, p).map(ExpressionOperatorParsers(_, env).operator).reduceOption(_ ||| _) match {
        case Some(parser) => parser | env.nextPriority(p).map(expression_cached).getOrElse(hostExpression)
        case None         => env.nextPriority(p).map(expression_cached).getOrElse(hostExpression)
      }
    }

    private val literal_cached: JPriority => LParser[IRExpression] = mutableHashMapMemo { p =>
      env.literalOperators(expected, p).map(LiteralOperatorParsers(_, env).operator).reduceOption(_ ||| _) match {
        case Some(parser) => parser | env.nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
        case None         => env.nextPriority(p).map(literal_cached).getOrElse(hostLiteral)
      }
    }
  }

  class JavaExpressionParsers private (env: Environment) {
    lazy val expression: HParser[IRExpression] = assignment | primary | cast

    lazy val assignment: HParser[IRAssignmentExpression] = simpleAssignment   // | += | -= | ...

    lazy val simpleAssignment: HParser[IRSimpleAssignmentExpression] = leftHandSide <~ '=' >> { left =>
      left.staticType match {
        case Some(t) => ExpressionParsers(t, env).expression ^^ { right => IRSimpleAssignmentExpression(left, right) }
        case None    => HParser.failure("type error")
      }
    }

    lazy val leftHandSide: HParser[IRLeftHandSide] = primary ^? { case e: IRLeftHandSide => e }

    lazy val primary: HParser[IRExpression] = arrayCreation | primaryNoNewArray

    lazy val arrayCreation: HParser[IRArrayCreation] = newArray | arrayInitializer

    lazy val newArray: HParser[IRNewArray] = ( "new" ~> typeParsers.componentType ) ~ ( '[' ~> intExpression <~ ']' ).+ ~ dimension ^^ {
      case componentType ~ length ~ dim => IRNewArray(componentType, length, dim)
    }

    lazy val arrayInitializer: HParser[IRArrayInitializer] = ( "new" ~> typeParsers.componentType ) ~ dimension1 >> {
      case componentType ~ dim => '{' ~> ExpressionParsers(componentType.array(dim - 1), env).expression.*(',') <~ ','.? <~ '}' ^^ {
        case components => IRArrayInitializer(componentType, dim, components)
      }
    }

    lazy val primaryNoNewArray: HParser[IRExpression] = methodCall | fieldAccess | arrayAccess | newExpression | abbreviatedMethodCall | classLiteral | variableRef | thisRef | abbreviatedFieldAccess | parenthesized

    lazy val newExpression: HParser[IRNewExpression] = ( "new" ~> typeParsers.metaArguments ) ~ typeParsers.objectType >>? {
      case metaArgs ~ constructType => constructType.findConstructor(env.clazz).flatMap(constructorCall(metaArgs, _)).reduceOption(_ | _)
    }

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

    lazy val instanceFieldAccess: HParser[IRInstanceFieldAccess] = primaryNoNewArray ~ ( '.' ~> identifier ) ^^? {
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

    lazy val arrayAccess: HParser[IRArrayAccess] = primaryNoNewArray ~ ( '[' ~> intExpression <~ ']' ) ^^ {
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

    lazy val intExpression = ExpressionParsers(compiler.typeLoader.int, env).expression

    lazy val dimension = ( '[' ~> ']' ).* ^^ { _.length }

    lazy val dimension1 = ( '[' ~> ']' ).+ ^^ { _.length }

    private def procedureCall [T] (procedure: JProcedure)(f: (Map[String, MetaArgument], List[IRExpression], List[IRContextRef]) => T): HParser[T] = {
      ArgumentParsers.arguments(procedure, env) ^^? { case (bind, args) => env.inferContexts(procedure, bind).map(f(bind, args, _)) }
    }

    private def procedureCall [T] (procedure: JProcedure, metaArgs: List[MetaArgument])(f: (Map[String, MetaArgument], List[IRExpression], List[IRContextRef]) => T): Option[HParser[T]] = {
      if (metaArgs.isEmpty) Some(procedureCall(procedure)(f))
      else for {
        bind     <- binding(procedure, metaArgs)
        contexts <- env.inferContexts(procedure, bind)
      } yield ArgumentParsers.arguments(procedure, bind, env) ^^ { f(bind, _, contexts) }
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

    private val typeParsers = TypeParsers(env.resolver)

    private def thisObject = env.thisType.map(IRThisRef)

    private def isThisRef (e: IRExpression) = thisObject.contains(e)

    private def binding (procedure: JProcedure, metaArgs: List[MetaArgument]): Option[Map[String, MetaArgument]] = {
      val metaParams = procedure.methodDef.signature.metaParams
      if (compiler.typeLoader.validTypeArgs(metaParams, metaArgs, procedure.env)) Some(metaParams.map(_.name).zip(metaArgs).toMap)
      else None
    }
  }

  object JavaLiteralParsers {
    def literal (expected: JType): LParser[IRExpression] = expected match {
      case compiler.typeLoader.char    => charLiteral
      case compiler.typeLoader.int     => intLiteral
      case compiler.typeLoader.long    => longLiteral
      case compiler.typeLoader.boolean => booleanLiteral
      case _ if compiler.typeLoader.stringType.exists(_ <:< expected) => stringLiteral
      case _ => LParser.failure("")
    }

    lazy val intLiteral: LParser[IRIntLiteral] = integerLiteral ^? { case n if Int.MinValue <= n && n <= Int.MaxValue => IRIntLiteral(n.toInt, compiler) }

    lazy val longLiteral: LParser[IRLongLiteral] = integerLiteral <~ ( elem('l') | elem('L') ) ^^ { value => IRLongLiteral(value, compiler) }

    lazy val integerLiteral: LParser[Long] = decimalLiteral | hexLiteral | binaryLiteral | octalLiteral | zeroLiteral

    lazy val decimalLiteral: LParser[Long] = nonZeroDigit ~ decimalDigit.* ^^ {
      case d ~ ds => ds.foldLeft(d.toLong) { _ * 10 + _ }
    }

    lazy val binaryLiteral: LParser[Long] = zero ~> ( elem('b') | elem('B') ) ~> binaryDigit.+ ^^ { _.foldLeft(0L) { _ * 2 + _ } }

    lazy val octalLiteral: LParser[Long] = zero ~> octalDigit.+ ^^ { _.foldLeft(0L) { _ * 8 + _ } }

    lazy val hexLiteral: LParser[Long] = zero ~> ( elem('x') | elem('X') ) ~> hexDigit.+ ^^ { _.foldLeft(0L) { _ * 16 + _ } }

    lazy val zeroLiteral: LParser[Long] = zero ^^^ 0L

    lazy val charLiteral: LParser[IRCharLiteral] = quote ~> (escapeSequence | except('\'')) <~ quote ^^ { value => IRCharLiteral(value, compiler) }

    lazy val booleanLiteral: LParser[IRBooleanLiteral] = word("true") ^^^ IRBooleanLiteral(true, compiler) | word("false") ^^^ IRBooleanLiteral(false, compiler)

    lazy val stringLiteral: LParser[IRStringLiteral] = dq ~> (escapeSequence | except('\"')).* <~ dq ^^ { cs => IRStringLiteral(cs.mkString, compiler) }

    lazy val escapeSequence = octalEscape | escape('b', '\b') | escape('f', '\f') | escape('n', '\n') | escape('r', '\r') | escape('t', '\t') | escape('\'', '\'') | escape('\"', '\"') | escape('\\', '\\')

    def escape (symbol: Char, character: Char) = backSlash ~> elem(symbol) ^^^ character

    lazy val octalEscape = octalEscape3 | octalEscape2 | octalEscape1

    lazy val octalEscape1 = backSlash ~> octalDigit ^^ { _.toChar }
    lazy val octalEscape2 = backSlash ~> ( octalDigit ~ octalDigit ) ^^ { case a ~ b => ((a << 3) + b).toChar }
    lazy val octalEscape3 = backSlash ~> ( quaternaryDigit ~ octalDigit ~ octalDigit ) ^^ { case a ~ b ~ c => ((a << 6) + (b << 3) + c).toChar }

    lazy val decimalDigit = elem("decimal digit", { ch => '0' <= ch && ch <= '9' }) ^^ { Character.digit(_, 10) }
    lazy val nonZeroDigit = elem("non zero digit", { ch => '1' <= ch && ch <= '9' }) ^^ { Character.digit(_, 10) }
    lazy val quaternaryDigit = elem("quaternary digit", { ch => '0' <= ch && ch <= '3' }) ^^ { Character.digit(_, 4) }
    lazy val octalDigit = elem("octal digit", { ch => '0' <= ch && ch <= '7' }) ^^ { Character.digit(_, 8) }
    lazy val hexDigit = elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ { Character.digit(_, 16) }
    lazy val binaryDigit  = zero ^^^ 0 | one ^^^ 1

    private lazy val zero = elem('0')
    private lazy val one = elem('1')
    private lazy val backSlash = elem('\\')
    private lazy val quote = elem('\'')
    private lazy val dq = elem('\"')

    def except(cs: Char*) = elem("", c => ! cs.contains(c) && c != EofCh)
  }

  class ExpressionOperatorParsers private (eop: ExpressionOperator, env: Environment) {
    import ArgumentParsers._

    lazy val operator: HParser[IRExpression] = constructParser(eop.syntax.pattern, eop.metaArgs, Nil)

    private def constructParser (pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], operands: List[IRExpression]): HParser[IRExpression] = pattern match {
      case JOperand(param) :: rest           => expression(param, binding, eop.method, env) >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JOptionalOperand(param) :: rest   => expression(param, binding, eop.method, env).?.mapOption { _.orElse(defaultArgument(param, eop.method, env)) } >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JRepetition0(param) :: rest       => rep0(param, binding) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JRepetition1(param) :: rest       => rep1(param, binding) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JMetaOperand(name, param) :: rest => expression(param, binding, eop.method, env) >> {
        ast => constructParser(rest, binding + (name -> ConcreteMetaValue(ast, param)), operands)
      }
      case JMetaName(value) :: rest          => metaValue(value, binding) ~> constructParser(rest, binding, operands)
      case JOperatorName(name) :: rest       => word(name).^ ~> constructParser(rest, binding, operands)
      case JAndPredicate(param) :: rest      => expression(param, binding, eop.method, env).& ~> constructParser(rest, binding, operands)
      case JNotPredicate(param) :: rest      => expression(param, binding, eop.method, env).! ~> constructParser(rest, binding, operands)
      case Nil                               => HParser.success(eop.semantics(binding, operands.reverse))
    }

    private def metaValue (mv: MetaArgument, binding: Map[String, MetaArgument]): HParser[MetaArgument] = mv match {
      case t: JRefType  => TypeParsers(env.resolver).refType ^? { case v if t == v => t }
      case w: JWildcard => TypeParsers(env.resolver).wildcard ^? { case v if w == v => w }
      case r: MetaVariableRef   => TypeParsers(env.resolver).metaVariable ^? { case v if r == v => r }
      case c: ConcreteMetaValue => expression(c.parameter, binding, eop.method, env) ^? { case v if c.ast == v => c }
      case _: MetaValueWildcard => HParser.failure("meta value wildcard cannot be placed in operator pattern")
    }

    private def rep0 (param: JParameter, binding: Map[String, MetaArgument]) = {
      HParser.repeat0((binding, List.empty[IRExpression])) {
        case ((b, l)) => expression(param, b, eop.method, env) ^^ { arg => (bind(param, arg, b), l :+ arg) }
      }
    }

    private def rep1 (param: JParameter, binding: Map[String, MetaArgument]) = {
      HParser.repeat1((binding, List.empty[IRExpression])) {
        case ((b, l)) => expression(param, b, eop.method, env) ^^ { arg => (bind(param, arg, b), l :+ arg) }
      }
    }
  }

  class LiteralOperatorParsers private (lop: LiteralOperator, env: Environment) {
    import ArgumentParsers._

    lazy val operator: LParser[IRExpression] = constructParser(lop.syntax.pattern, lop.metaArgs, Nil)

    private def constructParser (pattern: List[JSyntaxElement], binding: Map[String, MetaArgument], operands: List[IRExpression]): LParser[IRExpression] = pattern match {
      case JOperand(param) :: rest           => literal(param, binding, lop.method, env) >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JOptionalOperand(param) :: rest   => literal(param, binding, lop.method, env).?.mapOption { _.orElse(defaultArgument(param, lop.method, env)) } >> { arg =>
        constructParser(rest, bind(param, arg, binding), arg :: operands)
      }
      case JRepetition0(param) :: rest       => rep0(param, binding) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JRepetition1(param) :: rest       => rep1(param, binding) >> {
        case (bnd, args) => constructParser(rest, bnd, IRVariableArguments(args, param.genericType.bind(bnd)) :: operands)
      }
      case JMetaOperand(name, param) :: rest => literal(param, binding, lop.method, env) >> {
        ast => constructParser(rest, binding + (name -> ConcreteMetaValue(ast, param)), operands)
      }
      case JMetaName(value) :: rest          => metaValue(value, binding) ~> constructParser(rest, binding, operands)
      case JOperatorName(name) :: rest       => word(name) ~> constructParser(rest, binding, operands)
      case JAndPredicate(param) :: rest      => literal(param, binding, lop.method, env).& ~> constructParser(rest, binding, operands)
      case JNotPredicate(param) :: rest      => literal(param, binding, lop.method, env).! ~> constructParser(rest, binding, operands)
      case Nil                               => LParser.success(lop.semantics(binding, operands.reverse))
    }

    private def metaValue (mv: MetaArgument, binding: Map[String, MetaArgument]): LParser[MetaArgument] = mv match {
      case c: ConcreteMetaValue                            => literal(c.parameter, binding, lop.method, env) ^? { case v if c.ast == v => c }
      case _: JRefType | _: JWildcard | _: MetaVariableRef => LParser.failure("type name cannot be used in a literal")
      case _: MetaValueWildcard                            => LParser.failure("meta value wildcard cannot be placed in operator pattern")
    }

    private def rep0 (param: JParameter, binding: Map[String, MetaArgument]) = {
      LParser.repeat0((binding, List.empty[IRExpression])) {
        case ((b, l)) => literal(param, b, lop.method, env) ^^ { arg => (bind(param, arg, b), l :+ arg) }
      }
    }

    private def rep1 (param: JParameter, binding: Map[String, MetaArgument]) = {
      LParser.repeat1((binding, List.empty[IRExpression])) {
        case ((b, l)) => literal(param, b, lop.method, env) ^^ { arg => (bind(param, arg, b), l :+ arg) }
      }
    }
  }

  object ArgumentParsers {

    def arguments (procedure: JProcedure, environment: Environment): HParser[(Map[String, MetaArgument], List[IRExpression])] = {
      '(' ~> arguments_helper(procedure.parameterTypes, Map.empty, procedure, environment, Nil) <~ ')'
    }

    private def arguments_helper (parameters: List[JParameter], binding: Map[String, MetaArgument], procedure: JProcedure, environment: Environment, args: List[IRExpression]): HParser[(Map[String, MetaArgument], List[IRExpression])] = parameters match {
      case Nil           => HParser.success((binding, args))
      case param :: Nil  => expressionParser(param, binding, procedure, environment.highestPriority, environment) ^^ { arg => (bind(param, arg, binding), args :+ arg) }
      case param :: rest => expressionParser(param, binding, procedure, environment.highestPriority, environment) >> { arg => ',' ~> arguments_helper(rest, bind(param, arg, binding), procedure, environment, args :+ arg) }
    }

    def arguments (procedure: JProcedure, binding: Map[String, MetaArgument], environment: Environment): HParser[List[IRExpression]] = {
      '(' ~> HParser.sequence(procedure.parameterTypes.map(argument(_, binding, environment)), ',') <~ ')'
    }

    def argument (param: JParameter, binding: Map[String, MetaArgument], environment: Environment): HParser[IRExpression] = {
      param.genericType.bind(binding).map(expectedType => expressionParser(param, binding, expectedType, environment.highestPriority, environment)).getOrElse {
        HParser.failure("invalid expected type")
      }
    }

    def expression (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, environment: Environment): HParser[IRExpression] = {
      expressionParser(param, binding, procedure, priority(param, procedure, environment), environment)
    }

    def literal (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, environment: Environment): LParser[IRExpression] = {
      literalParser(param, binding, procedure, priority(param, procedure, environment), environment)
    }

    def bind (param: JParameter, arg: IRExpression, binding: Map[String, MetaArgument]) = binding ++ arg.staticType.flatMap(compiler.unifier.infer(_, param.genericType)).getOrElse(Map.empty)

    def defaultArgument (param: JParameter, procedure: JProcedure, environment: Environment) = for {
      name   <- param.defaultArg
      method <- procedure.declaringClass.classModule.findMethod(name, environment.clazz).find(_.erasedParameterTypes == Nil)
    } yield IRStaticMethodCall(Map.empty, method, Nil, Nil)

    private def expressionParser (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, priority: Option[JPriority], environment: Environment): HParser[IRExpression] = {
      expected(param, binding, procedure).map(expectedType => expressionParser(param, binding, expectedType, priority, environment)).getOrElse {
        HParser.failure("fail to parse argument expression")
      }
    }

    private def expressionParser (param: JParameter, binding: Map[String, MetaArgument], expectedType: JType, priority: Option[JPriority], environment: Environment): HParser[IRExpression] = {
      IRContextRef.createRefs(param.contexts, binding).map(contexts => ExpressionParsers(expectedType, environment.withContexts(contexts)).expression(priority).map { arg =>
        if (contexts.nonEmpty) IRContextualArgument(arg, contexts)
        else arg
      }).getOrElse {
        HParser.failure("fail to parse argument expression")
      }
    }

    private def literalParser (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure, priority: Option[JPriority], environment: Environment): LParser[IRExpression] = {
      expected(param, binding, procedure).flatMap { expectedType =>
        IRContextRef.createRefs(param.contexts, binding).map(contexts => ExpressionParsers(expectedType, environment.withContexts(contexts)).literal(priority).map { arg =>
          if (contexts.nonEmpty) IRContextualArgument(arg, contexts)
          else arg
        })
      }.getOrElse {
        LParser.failure("fail to parse argument expression")
      }
    }

    private def expected (param: JParameter, binding: Map[String, MetaArgument], procedure: JProcedure): Option[JType] = {
      unbound(param.genericType.unbound(binding).toList, binding, procedure).flatMap(param.genericType.bind)
    }

    private def priority (param: JParameter, procedure: JProcedure, environment: Environment): Option[JPriority] = {
      param.priority.orElse(procedure.syntax.flatMap(syntax => environment.nextPriority(syntax.priority)))
    }

    private def unbound (names: List[String], binding: Map[String, MetaArgument], procedure: JProcedure): Option[Map[String, MetaArgument]] = names match {
      case name :: rest => procedure.metaParameters.get(name).flatMap(mp => unboundMetaParameter(name, binding, mp.metaType, mp.bounds)) match {
        case Some(arg) => unbound(rest, binding + (name -> arg), procedure)
        case None      => None
      }
      case Nil => Some(binding)
    }

    import scalaz.Scalaz._
    private def unboundMetaParameter (name: String, binding: Map[String, MetaArgument], metaType: JTypeSignature, bounds: List[JTypeSignature]): Option[MetaArgument] = {
      if (metaType == JTypeSignature.typeTypeSig) bounds.traverse(compiler.typeLoader.fromTypeSignature_RefType(_, binding)).map(JUnboundTypeVariable(name, _, compiler))
      else compiler.typeLoader.fromTypeSignature_RefType(metaType, binding).map(MetaValueWildcard)
    }
  }

  class TypeParsers private (resolver: NameResolver) {
    lazy val metaArguments: HParser[List[MetaArgument]] = ( '<' ~> metaValue.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }
    lazy val metaValue: HParser[MetaArgument] = wildcard | metaVariable | refType
    lazy val typeName: HParser[JType] = primitiveTypeName | refType
    lazy val componentType: HParser[JType] = primitiveTypeName | objectType
    lazy val refType: HParser[JRefType] = arrayType | typeVariable | objectType
    lazy val objectType: HParser[JObjectType] = className ~ metaArguments ^^? {
      case clazz ~ args => clazz.objectType(args)
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
      case "byte"    => compiler.typeLoader.byte
      case "char"    => compiler.typeLoader.char
      case "double"  => compiler.typeLoader.double
      case "float"   => compiler.typeLoader.float
      case "int"     => compiler.typeLoader.int
      case "long"    => compiler.typeLoader.long
      case "short"   => compiler.typeLoader.short
      case "boolean" => compiler.typeLoader.boolean
      case "void"    => compiler.typeLoader.void
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

  object JavaExpressionParsers {
    def apply (env: Environment): JavaExpressionParsers = cached(env)
    private val cached : Environment => JavaExpressionParsers = mutableHashMapMemo { e => new JavaExpressionParsers(e) }
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

  private lazy val word_cached: String => LParser[String] = mutableHashMapMemo { cs => cs.foldRight(LParser.success(cs)) { (ch, r) => elem(ch) ~> r } }
}
