package phenan.prj.declaration

import phenan.prj.combinator.TwoLevelParsers
import phenan.prj.exception.ParseException
import java.io.Reader

import scala.language.implicitConversions
import scala.util._
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input._
import CharArrayReader.EofCh

object DeclarationParsers extends TwoLevelParsers {
  override type Elem = DToken

  def parse [T] (parser: HParser[T], src: String): ParseResult[T] = parser(scanner(src))
  def parse [T] (parser: HParser[T], reader: Reader): ParseResult[T] = parser(scanner(reader))

  def tryParse [T] (parser: HParser[T], reader: Reader, file: String): Try[T] = parse(parser, reader) match {
    case ParseSuccess(result, _) => Success(result)
    case ParseFailure(msg, _)    => Failure(ParseException(s"[ parse error ] at $file \n $msg"))
  }

  private def scanner (src: String): DeclarationScanners.Scanner = new DeclarationScanners.Scanner(new DeclarationPreprocessor.Scanner(src))
  private def scanner (reader: Reader): DeclarationScanners.Scanner = new DeclarationScanners.Scanner(new DeclarationPreprocessor.Scanner(StreamReader(reader)))

  lazy val compilationUnit: HParser[CompilationUnit] = header ~ moduleDeclaration.* ^^ {
    case head ~ body => CompilationUnit(head, body)
  }

  lazy val header: HParser[Header] = packageDeclaration.? ~ importDeclaration.* ^^ {
    case pack ~ imports => Header(pack, imports)
  }

  lazy val packageDeclaration: HParser[PackageDeclaration] = "package" ~> qualifiedName <~ ';' ^^ PackageDeclaration

  lazy val importDeclaration: HParser[ImportDeclaration] = importStatic | importDSLs | importPackage | importClass

  lazy val importStatic: HParser[StaticImportDeclaration] = importStaticMember | importStaticStar

  lazy val importStaticMember: HParser[ImportStaticMemberDeclaration] = "import" ~> "static" ~> qualifiedName <~ ';' ^^ ImportStaticMemberDeclaration

  lazy val importStaticStar: HParser[ImportStaticOnDemandDeclaration] = "import" ~> "static" ~> qualifiedName <~ '.' <~ '*' <~ ';' ^^ ImportStaticOnDemandDeclaration

  lazy val importDSLs: HParser[ImportDSLsDeclaration] = importDSLs_Simple | importDSLs_Constraints

  lazy val importDSLs_Simple: HParser[ImportDSLsDeclaration] = "import" ~> ( "dsl" | "dsls" ) ~> qualifiedName.+(',') <~ ';' ^^ { dsls => ImportDSLsDeclaration(dsls, Nil) }

  lazy val importDSLs_Constraints: HParser[ImportDSLsDeclaration] = "import" ~> ( "dsl" | "dsls" ) ~> qualifiedName.+(',') ~ ( '{' ~> constraint.*(';') <~ '}' ) ^^ {
    case dsls ~ constraints => ImportDSLsDeclaration(dsls, constraints)
  }

  lazy val importClass: HParser[ClassImportDeclaration] = "import" ~> qualifiedName <~ ';' ^^ ClassImportDeclaration

  lazy val importPackage: HParser[PackageImportDeclaration] = "import" ~> qualifiedName <~ '.' <~ '*' <~ ';' ^^ PackageImportDeclaration


  /* module declarations */

  lazy val moduleDeclaration: HParser[ModuleDeclaration] = classDeclaration | enumDeclaration | interfaceDeclaration | annotationDeclaration | dslDeclaration

  lazy val classDeclaration: HParser[ClassDeclaration] = modifiers ~ ( "class" ~> identifier ) ~ metaParameters ~ ( "extends" ~> typeName ).? ~ implementsInterfaces >> {
    case mods ~ name ~ mps ~ sup ~ ifs => ClassMemberParsers(name).classBody ^^ { body => ClassDeclaration(mods, name, mps, sup, ifs, body) }
  }

  lazy val enumDeclaration: HParser[EnumDeclaration] = modifiers ~ ( "enum" ~> identifier ) ~ implementsInterfaces >> {
    case mods ~ name ~ ifs => ClassMemberParsers(name).enumBody ^^ {
      case cns ~ members => EnumDeclaration(mods, name, ifs, cns, members)
    }
  }

  lazy val interfaceDeclaration: HParser[InterfaceDeclaration] = modifiers ~ ( "interface" ~> identifier ) ~ metaParameters ~ extendsInterfaces ~ ( '{' ~> interfaceMember.* <~ '}' ) ^^ {
    case mods ~ name ~ mps ~ ifs ~ body => InterfaceDeclaration(mods, name, mps, ifs, body)
  }

  lazy val annotationDeclaration: HParser[AnnotationDeclaration] = modifiers ~ ( '@' ~> "interface" ~> identifier ) ~ ( '{' ~> annotationMember.* <~ '}' ) ^^ {
    case mods ~ name ~ body => AnnotationDeclaration(mods, name, body)
  }

  lazy val dslDeclaration: HParser[DSLDeclaration] = modifiers ~ ( "dsl" ~> identifier ) ~ metaParameters ~ mixinDSLs >> {
    case mods ~ name ~ mps ~ mixins => ClassMemberParsers(name).dslBody ^^ { DSLDeclaration(mods, name, mps, mixins, _) }
  }

  lazy val implementsInterfaces: HParser[List[TypeName]] = ( "implements" ~> typeName.+(',')).? ^^ { _.getOrElse(Nil) }

  lazy val extendsInterfaces: HParser[List[TypeName]] = ( "extends" ~> typeName.+(',')).? ^^ { _.getOrElse(Nil) }

  lazy val mixinDSLs: HParser[List[QualifiedName]] = ( "with" ~> qualifiedName.+(',') ).? ^^ { _.getOrElse(Nil) }


  /* members*/

  lazy val interfaceMember: HParser[InterfaceMember] = methodDeclaration | fieldDeclaration | moduleDeclaration

  lazy val annotationMember: HParser[AnnotationMember] = annotationElementDeclaration | fieldDeclaration | moduleDeclaration

  lazy val instanceInitializer: HParser[InstanceInitializer] = block ^^ InstanceInitializer

  lazy val staticInitializer: HParser[StaticInitializer] = "static" ~> block ^^ StaticInitializer

  lazy val methodDeclaration: HParser[MethodDeclaration] = modifiers ~ metaParameters ~ requires_Turnstile.? ~ typeName ~ identifier ~ formalParameters ~ clause.* ~ methodBody ^^ {
    case mods ~ mps ~ req ~ ret ~ name ~ params ~ clauses ~ body => MethodDeclaration(mods, mps, ret, name, params, clauses ++ req, body)
  }

  lazy val fieldDeclaration: HParser[FieldDeclaration] = modifiers ~ typeName ~ declarator.+(',') <~ ';' ^^ {
    case mods ~ t ~ ds => FieldDeclaration(mods, t, ds)
  }

  lazy val annotationElementDeclaration: HParser[AnnotationElementDeclaration] = modifiers ~ typeName ~ ( identifier <~ '(' <~ ')' ) ~ emptyBrackets ~ ( "default" ~> annotationElement ).? <~ ';' ^^ {
    case mods ~ tn ~ name ~ dim ~ default => AnnotationElementDeclaration(mods, tn, name, dim, default)
  }

  lazy val operatorDeclaration: HParser[OperatorDeclaration] = ( identifier <~ ':' ).? ~ modifiers ~ metaParameters ~ requires_Turnstile.? ~ typeName ~ returnBound.* ~ priority ~ syntaxElement.+ ~ formalParameters ~ clause.* ~ methodBody ^^ {
    case label ~ mods ~ mps ~ req ~ tn ~ bounds ~ pri ~ syn ~ params ~ clauses ~ body => OperatorDeclaration(label, mods, mps, tn, bounds, pri, syn, params, clauses ++ req, body)
  }

  lazy val prioritiesDeclaration: HParser[PrioritiesDeclaration] = ("priority" | "priorities") ~> identifier.*(',') ~ ( '{' ~> constraint.*(';') <~ '}' ) ^^ {
    case names ~ constraints => PrioritiesDeclaration(names, constraints)
  }

  lazy val constraint: HParser[List[QualifiedName]] = ascendingConstraint | descendingConstraint

  lazy val ascendingConstraint: HParser[List[QualifiedName]] = ( qualifiedName <~ '<' ) ~ qualifiedName.+('<') ^^ {
    case head ~ tail => ( head :: tail ).reverse
  }

  lazy val descendingConstraint: HParser[List[QualifiedName]] = ( qualifiedName <~ '>' ) ~ qualifiedName.+('>') ^^ {
    case head ~ tail => head :: tail
  }

  lazy val formalParameters: HParser[List[FormalParameter]] = '(' ~> formalParameter.*(',') <~ ')'

  lazy val formalParameter: HParser[FormalParameter] = modifiers ~ parameterType ~ dots ~ identifier ~ emptyBrackets ~ ( '=' ~> expression ).? ~ scopeDeclarator.? ^^ {
    case mods ~ pt ~ ds ~ name ~ dim ~ init ~ scope => FormalParameter(mods, pt, ds, name, dim, init, scope.getOrElse(Nil))
  }

  lazy val scopeDeclarator: HParser[List[TypeName]] =
    "scope" ~> "for" ~> typeName ^^ { List(_) } | "scope" ~> "for" ~> '{' ~> typeName.+(',') <~ '}'

  lazy val declarator: HParser[VariableDeclarator] = identifier ~ emptyBrackets ~ ( '=' ~> expression ).? ^^ {
    case name ~ dim ~ init => VariableDeclarator(name, dim, init)
  }

  lazy val syntaxElement: HParser[SyntaxElement] = operatorName | regexName | optionalOperand | repetition0 | repetition1 | operand | metaValueRef | andPredicate | notPredicate

  lazy val operatorName: HParser[OperatorName] = elem[StrLiteral].^ ^^ { lit => OperatorName(lit.value) }
  lazy val regexName: HParser[RegexName] = (underscore ~> elem('%')).^ ~> elem[StrLiteral].^ ^^ { lit => RegexName(lit.value) }

  lazy val optionalOperand: HParser[OptionalOperand] = (underscore ~> elem('?')).^ ~> priority ^^ OptionalOperand
  lazy val repetition0: HParser[Repetition0] = (underscore ~> elem('*')).^ ~> priority ^^ Repetition0
  lazy val repetition1: HParser[Repetition1] = (underscore ~> elem('+')).^ ~> priority ^^ Repetition1
  lazy val operand: HParser[Operand] = underscore.^ ~> priority ^^ Operand

  lazy val metaValueRef: HParser[MetaValueRef] = identifier ~ priority ^^ {
    case id ~ pri => MetaValueRef(id, pri)
  }

  lazy val andPredicate: HParser[AndPredicate] = ('&' ~> typeName) ~ priority ^^ {
    case t ~ p => AndPredicate(t, p)
  }
  lazy val notPredicate: HParser[NotPredicate] = ('!' ~> typeName) ~ priority ^^ {
    case t ~ p => NotPredicate(t, p)
  }

  lazy val underscore: LParser[DToken] = elem(Identifier("_"))

  lazy val dots: HParser[Boolean] = (elem('.') ~> elem('.') ~> elem('.')).?.map(_.nonEmpty).^

  lazy val priority: HParser[Option[QualifiedName]] = ( '[' ~> qualifiedName <~ ']' ).?

  lazy val emptyBrackets: HParser[Int] = ('[' ~> ']').* ^^ { _.size }

  lazy val clause: HParser[MethodClause] = throwsClause | activatesClause | deactivatesClause | requiresClause

  lazy val throwsClause: HParser[ThrowsClause] = "throws" ~> typeName.+(',') ^^ ThrowsClause

  lazy val activatesClause: HParser[ActivatesClause] = "activates" ~> typeName.+(',') ^^ ActivatesClause

  lazy val deactivatesClause: HParser[DeactivatesClause] = "deactivates" ~> typeName.+(',') ^^ DeactivatesClause

  lazy val requiresClause: HParser[RequiresClause] = "requires" ~> typeName.+(',') ^^ RequiresClause

  lazy val requires_Turnstile: HParser[RequiresClause] = ( multipleTypeNames | typeName ^^ { List(_) } ) <~ turnstileSymbol.^ ^^ RequiresClause

  lazy val multipleTypeNames: HParser[List[TypeName]] = '{' ~> typeName.+(',') <~ '}'

  lazy val turnstileSymbol: LParser[_] = elem('|') ~> elem('-')

  lazy val methodBody: HParser[Option[BlockSnippet]] = block ^^ { Some(_) } | ';' ^^^ None

  case class ClassMemberParsers (className: String) {
    lazy val classBody: HParser[List[ClassMember]] = '{' ~> classMember.* <~ '}'

    lazy val enumBody: HParser[List[EnumConstant] ~ List[ClassMember]] = '{' ~> ( enumConstants ~ enumMembers ) <~ '}'

    lazy val dslBody: HParser[List[DSLMember]] = '{' ~> dslMember.* <~ '}'

    lazy val classMember: HParser[ClassMember] =
      instanceInitializer | staticInitializer | constructorDeclaration | methodDeclaration | fieldDeclaration | moduleDeclaration

    lazy val enumMembers: HParser[List[ClassMember]] = ( ';' ~> classMember.* ).? ^^ { _.getOrElse(Nil) }

    lazy val enumConstants: HParser[List[EnumConstant]] = enumConstant.*(',') <~ ','.?

    lazy val enumConstant: HParser[EnumConstant] = annotation.* ~ identifier ~ ( '(' ~> expression.*(',') <~ ')' ).? ~ classBody.? ^^ {
      case as ~ name ~ args ~ body => EnumConstant(as, name, args.getOrElse(Nil), body.getOrElse(Nil))
    }

    lazy val dslMember: HParser[DSLMember] = prioritiesDeclaration | constructorDeclaration | operatorDeclaration | fieldDeclaration

    lazy val constructorDeclaration: HParser[ConstructorDeclaration] = modifiers ~ metaParameters ~ className ~ formalParameters ~ clause.* ~ block ^^ {
      case mods ~ mps ~ _ ~ params ~ clauses ~ body => ConstructorDeclaration(mods, mps, params, clauses, body)
    }
  }

  /* types */

  lazy val parameterType: HParser[ParameterType] = contextualType | typeName

  lazy val contextualType: HParser[ContextualType] = ( typeName <~ turnstileSymbol.^ ) ~ parameterType ^^ {
    case ct ~ pt => ContextualType(ct, pt)
  }

  lazy val returnBound: HParser[TypeName] = '^' ~> typeName

  lazy val typeName: HParser[TypeName] = className ~ typeArguments ~ emptyBrackets ^^ {
    case name ~ args ~ dim => TypeName(name, args, dim)
  }

  lazy val className: HParser[QualifiedName] = ( "class".! ~> identifier ).+('.') ^^ QualifiedName

  lazy val metaParameters: HParser[List[MetaParameter]] = ( '<' ~> metaParameter.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }

  lazy val metaParameter: HParser[MetaParameter] = metaValueParameter | typeParameter

  lazy val metaValueParameter: HParser[MetaValueParameter] = identifier ~ ( ':' ~> typeName ) ^^ {
    case name ~ metaType => MetaValueParameter(name, metaType)
  }

  lazy val typeParameter: HParser[TypeParameter] = identifier ~ ( "extends" ~> typeName.+('&') ).? ^^ {
    case name ~ bounds => TypeParameter(name, bounds.getOrElse(Nil))
  }

  lazy val typeArguments: HParser[List[TypeArgument]] = ( '<' ~> typeArgument.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }

  lazy val typeArgument: HParser[TypeArgument] = wildcardType | typeName

  lazy val wildcardType: HParser[TypeArgument] = wildcardExtends | wildcardSuper | unboundWildcard

  lazy val wildcardExtends: HParser[UpperBoundWildcardType] = '?' ~> "extends" ~> typeName ^^ { bound => UpperBoundWildcardType(bound) }

  lazy val wildcardSuper: HParser[LowerBoundWildcardType] = '?' ~> "super" ~> typeName ^^ { bound => LowerBoundWildcardType(bound) }

  lazy val unboundWildcard: HParser[UnboundWildcardType.type] = '?' ^^^ UnboundWildcardType


  /* modifiers and annotations */

  lazy val modifiers: HParser[List[Modifier]] = (annotation | modifier).*

  lazy val modifier: HParser[Modifier] =
    "public" ^^^ PublicModifier | "private" ^^^ PrivateModifier | "protected" ^^^ ProtectedModifier |
      "static" ^^^ StaticModifier | "final" ^^^ FinalModifier | "synchronized" ^^^ SynchronizedModifier |
      "volatile" ^^^ VolatileModifier | "transient" ^^^ TransientModifier | "native" ^^^ NativeModifier |
      "abstract" ^^^ AbstractModifier | "strictfp" ^^^ StrictFPModifier | "pure" ^^^ PureModifier |
      "literal" ^^^ LiteralModifier | "exact" ^^^ ExactModifier

  lazy val annotation: HParser[Annotation] = fullAnnotation | singleElementAnnotation | markerAnnotation

  lazy val fullAnnotation: HParser[FullAnnotation] = annotationName ~ ( '(' ~> fullAnnotationArgument.*(',') <~ ')' ) ^^ {
    case name ~ args => FullAnnotation(name, args.toMap)
  }

  lazy val fullAnnotationArgument: HParser[(String, AnnotationElement)] = ( identifier <~ '=' ) ~ annotationElement ^^ {
    case name ~ arg => (name, arg)
  }

  lazy val singleElementAnnotation: HParser[SingleElementAnnotation] = annotationName ~ ( '(' ~> annotationElement <~ ')' ) ^^ {
    case name ~ elem => SingleElementAnnotation(name, elem)
  }

  lazy val markerAnnotation: HParser[MarkerAnnotation] = annotationName ^^ MarkerAnnotation

  lazy val annotationName: HParser[QualifiedName] = '@' ~> "interface".! ~> qualifiedName

  lazy val annotationElement: HParser[AnnotationElement] = annotation | arrayOfAnnotationElement | annotationExpression | enumConstant

  lazy val arrayOfAnnotationElement: HParser[ArrayOfAnnotationElement] = '{' ~> annotationElement.*(',') <~ ','.? <~ '}' ^^ ArrayOfAnnotationElement


  /* body code snippet */

  lazy val block: HParser[BlockSnippet] = positioned(blockCode.^ ^^ {
    code => BlockSnippet(code.map(_.raw).mkString)
  })

  lazy val expression: HParser[ExpressionSnippet] = positioned(expressionCode.*.^ ^^ {
    code => ExpressionSnippet(code.flatten.map(_.raw).mkString)
  })

  lazy val expressionCode: LParser[List[DToken]] = blockCode | parenthesizedCode | without('(', ')', '{', '}', ',', ';').+

  lazy val parenthesizedCode: LParser[List[DToken]] = elem('(') ~> argumentCode.* <~ elem(')') ^^ { Symbol('(') +: _.flatten :+ Symbol(')') }

  lazy val argumentCode: LParser[List[DToken]] = parenthesizedCode | without('(', ')').+

  lazy val blockCode: LParser[List[DToken]] = elem('{') ~> blockStatementCode.* <~ elem('}') ^^ { Symbol('{') +: _.flatten :+ Symbol('}') }

  lazy val blockStatementCode: LParser[List[DToken]] = blockCode | without('{', '}').+

  /* annotation expression */

  lazy val annotationExpression: HParser[AnnotationExpression] = stringLiteral | classLiteral

  lazy val stringLiteral: HParser[StringLiteralExpression] = elem[StrLiteral].^ ^^ { s => StringLiteralExpression(s.value) }

  lazy val classLiteral: HParser[ClassLiteralExpression] = typeName ^^ ClassLiteralExpression

  lazy val enumConstant: HParser[EnumConstantElement] = qualifiedName ^^ { name => EnumConstantElement(name.names) }

  /* primary */

  lazy val qualifiedName: HParser[QualifiedName] = identifier.+('.') ^^ QualifiedName

  lazy val identifier: HParser[String] = accept("identifier", { case Identifier(id) => id }).^

  lazy val delimiter: LParser[List[Whitespace]] = elem[Whitespace].*

  private implicit def keyword (word: String): HParser[DToken] = elem(Identifier(word)).^
  private implicit def symbol (ch: Char): HParser[DToken] = elem(Symbol(ch)).^

  private def elem (ch: Char): LParser[DToken] = elem(Symbol(ch))
  private def without (ts: Char*): LParser[DToken] = elem("without " + ts.mkString(","), {
    case Symbol(s) if ts.contains(s) => false
    case _: ErrorToken => false
    case _ => true
  })
}

object DeclarationScanners extends Scanners {
  override type Token = DToken

  lazy val token: Parser[DToken] = identifier | space | charLiteral | stringLiteral | symbol

  lazy val whitespace: Parser[Any] = (lineComment | blockComment).*

  lazy val identifier: Parser[Identifier] = elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).* ^^ {
    case s ~ ps => Identifier(s +: ps.mkString)
  }

  lazy val space: Parser[Whitespace] = elem("white space", Character.isWhitespace) ^^ { Whitespace(_) }

  lazy val charLiteral: Parser[CharLiteral] = '\'' ~> (escapeSequence | except('\'')) <~ '\'' ^^ { CharLiteral(_) }

  lazy val stringLiteral: Parser[StrLiteral] = '\"' ~> (escapeSequence | except('\"')).* <~ '\"' ^^ { cs => StrLiteral(cs.mkString) }

  lazy val symbol: Parser[Symbol] = except() ^^ { Symbol(_) }

  lazy val escapeSequence: Parser[Char] = octalEscape | escape('b', '\b') | escape('f', '\f') | escape('n', '\n') | escape('r', '\r') | escape('t', '\t') | escape('\'', '\'') | escape('\"', '\"') | escape('\\', '\\')

  def escape (symbol: Char, character: Char): Parser[Char] = '\\' ~> symbol ^^^ character

  lazy val octalEscape: Parser[Char] = octalEscape3 | octalEscape2 | octalEscape1

  lazy val octalEscape1: Parser[Char] = '\\' ~> octalDigit ^^ { _.toChar }
  lazy val octalEscape2: Parser[Char] = '\\' ~> ( octalDigit ~ octalDigit ) ^^ { case a ~ b => ((a << 3) + b).toChar }
  lazy val octalEscape3: Parser[Char] = '\\' ~> ( quaternaryDigit ~ octalDigit ~ octalDigit ) ^^ { case a ~ b ~ c => ((a << 6) + (b << 3) + c).toChar }

  lazy val quaternaryDigit: Parser[Int] = elem("quaternary digit", { ch => '0' <= ch && ch <= '3' }) ^^ { Character.digit(_, 4) }
  lazy val octalDigit: Parser[Int] = elem("octal digit", { ch => '0' <= ch && ch <= '7' }) ^^ { Character.digit(_, 8) }

  lazy val lineComment: Parser[List[Char]] = '/' ~> '/' ~> except('\n').*
  lazy val blockComment: Parser[List[Char]] = '/' ~> '*' ~> ( except('*') | '*' <~ not('/') ).* <~ '*' <~ '/'

  override def errorToken(msg: String): DToken = ErrorToken(msg)

  def except(cs: Char*): Parser[Char] = elem("", c => ! cs.contains(c) && c != EofCh)
}

object DeclarationPreprocessor extends Scanners {
  override type Token = Char

  lazy val token: Parser[Char] = unicodeEscape | elem("any", _ => true)

  lazy val unicodeEscape: Parser[Char] = '\\' ~> elem('u').+ ~> ( hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ) ^^ {
    case a ~ b ~ c ~ d => ((a << 12) + (b << 8) + (c << 4) + d).toChar
  }

  lazy val hexDigit: Parser[Int] = elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ { Character.digit(_, 16) }

  val whitespace: Parser[Unit] = success(())

  def errorToken(msg: String): Char = EofCh
}
