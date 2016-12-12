package phenan.prj.declaration

import phenan.prj.combinator.TwoLevelParsers
import phenan.prj.exception.ParseException

import scala.collection.immutable.PagedSeq
import scala.language.implicitConversions
import scala.util._
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input._
import CharArrayReader.EofCh

object DeclarationParsers extends TwoLevelParsers {
  override type Elem = DToken

  def parse [T] (parser: HParser[T], src: String): ParseResult[T] = parser(scanner(src))
  def parse [T] (parser: HParser[T], seq: PagedSeq[Char]): ParseResult[T] = parser(scanner(seq))

  def tryParse [T] (parser: HParser[T], seq: PagedSeq[Char], file: String): Try[T] = parse(parser, seq) match {
    case ParseSuccess(result, _) => Success(result)
    case ParseFailure(msg, _)    => Failure(ParseException("[ parse error ] at " + file + "\n" + msg))
  }

  private def scanner (src: String): DeclarationScanners.Scanner = new DeclarationScanners.Scanner(new DeclarationPreprocessor.Scanner(src))
  private def scanner (seq: PagedSeq[Char]): DeclarationScanners.Scanner = new DeclarationScanners.Scanner(new DeclarationPreprocessor.Scanner(new PagedSeqReader(seq)))

  lazy val compilationUnit = header ~ moduleDeclaration.* ^^ {
    case head ~ body => CompilationUnit(head, body)
  }

  lazy val header: HParser[Header] = packageDeclaration.? ~ importDeclaration.* ^^ {
    case pack ~ imports => Header(pack, imports)
  }

  lazy val packageDeclaration = "package" ~> qualifiedName <~ ';' ^^ PackageDeclaration

  lazy val importDeclaration = importStatic | importDSLs | importPackage | importClass

  lazy val importStatic = importStaticMember | importStaticStar

  lazy val importStaticMember = "import" ~> "static" ~> qualifiedName <~ ';' ^^ ImportStaticMemberDeclaration

  lazy val importStaticStar = "import" ~> "static" ~> qualifiedName <~ '.' <~ '*' <~ ';' ^^ ImportStaticOnDemandDeclaration

  lazy val importDSLs = importDSLs_Simple | importDSLs_Constraints

  lazy val importDSLs_Simple = "import" ~> ( "dsl" | "dsls" ) ~> qualifiedName.+(',') <~ ';' ^^ { dsls => ImportDSLsDeclaration(dsls, Nil) }

  lazy val importDSLs_Constraints = "import" ~> ( "dsl" | "dsls" ) ~> qualifiedName.+(',') ~ ( '{' ~> constraint.*(';') <~ '}' ) ^^ {
    case dsls ~ constraints => ImportDSLsDeclaration(dsls, constraints)
  }

  lazy val importClass = "import" ~> qualifiedName <~ ';' ^^ ClassImportDeclaration

  lazy val importPackage = "import" ~> qualifiedName <~ '.' <~ '*' <~ ';' ^^ PackageImportDeclaration


  /* module declarations */

  lazy val moduleDeclaration: HParser[ModuleDeclaration] = classDeclaration | enumDeclaration | interfaceDeclaration | annotationDeclaration | dslDeclaration

  lazy val classDeclaration = modifiers ~ ( "class" ~> identifier ) ~ metaParameters ~ ( "extends" ~> typeName ).? ~ implementsInterfaces >> {
    case mods ~ name ~ mps ~ sup ~ ifs => ClassMemberParsers(name).classBody ^^ { body => ClassDeclaration(mods, name, mps, sup, ifs, body) }
  }

  lazy val enumDeclaration = modifiers ~ ( "enum" ~> identifier ) ~ implementsInterfaces >> {
    case mods ~ name ~ ifs => ClassMemberParsers(name).enumBody ^^ {
      case cns ~ members => EnumDeclaration(mods, name, ifs, cns, members)
    }
  }

  lazy val interfaceDeclaration = modifiers ~ ( "interface" ~> identifier ) ~ metaParameters ~ extendsInterfaces ~ ( '{' ~> interfaceMember.* <~ '}' ) ^^ {
    case mods ~ name ~ mps ~ ifs ~ body => InterfaceDeclaration(mods, name, mps, ifs, body)
  }

  lazy val annotationDeclaration = modifiers ~ ( '@' ~> "interface" ~> identifier ) ~ ( '{' ~> annotationMember.* <~ '}' ) ^^ {
    case mods ~ name ~ body => AnnotationDeclaration(mods, name, body)
  }

  lazy val dslDeclaration = modifiers ~ ( "dsl" ~> identifier ) ~ metaParameters ~ mixinDSLs >> {
    case mods ~ name ~ mps ~ mixins => ClassMemberParsers(name).dslBody ^^ { DSLDeclaration(mods, name, mps, mixins, _) }
  }

  lazy val implementsInterfaces = ( "implements" ~> typeName.+(',')).? ^^ { _.getOrElse(Nil) }

  lazy val extendsInterfaces = ( "extends" ~> typeName.+(',')).? ^^ { _.getOrElse(Nil) }

  lazy val mixinDSLs = ( "with" ~> qualifiedName.+(',') ).? ^^ { _.getOrElse(Nil) }


  /* members*/

  lazy val interfaceMember: HParser[InterfaceMember] = methodDeclaration | fieldDeclaration | moduleDeclaration

  lazy val annotationMember: HParser[AnnotationMember] = annotationElementDeclaration | fieldDeclaration | moduleDeclaration

  lazy val instanceInitializer = block ^^ InstanceInitializer

  lazy val staticInitializer = "static" ~> block ^^ StaticInitializer

  lazy val methodDeclaration = modifiers ~ metaParameters ~ typeName ~ identifier ~ formalParameters ~ clause.* ~ methodBody ^^ {
    case mods ~ mps ~ ret ~ name ~ params ~ clauses ~ body => MethodDeclaration(mods, mps, ret, name, params, clauses, body)
  }

  lazy val fieldDeclaration = modifiers ~ typeName ~ declarator.+(',') <~ ';' ^^ {
    case mods ~ t ~ ds => FieldDeclaration(mods, t, ds)
  }

  lazy val annotationElementDeclaration = modifiers ~ typeName ~ ( identifier <~ '(' <~ ')' ) ~ emptyBrackets ~ ( "default" ~> annotationElement ).? <~ ';' ^^ {
    case mods ~ tn ~ name ~ dim ~ default => AnnotationElementDeclaration(mods, tn, name, dim, default)
  }

  lazy val operatorDeclaration = ( identifier <~ ':' ).? ~ modifiers ~ metaParameters ~ typeName ~ returnBound.* ~ priority ~ syntaxElement.+ ~ formalParameters ~ clause.* ~ methodBody ^^ {
    case label ~ mods ~ mps ~ tn ~ bounds ~ pri ~ syn ~ params ~ clauses ~ body => OperatorDeclaration(label, mods, mps, tn, bounds, pri, syn, params, clauses, body)
  }

  lazy val prioritiesDeclaration = ("priority" | "priorities") ~> identifier.*(',') ~ ( '{' ~> constraint.*(';') <~ '}' ) ^^ {
    case names ~ constraints => PrioritiesDeclaration(names, constraints)
  }

  lazy val constraint = ascendingConstraint | descendingConstraint

  lazy val ascendingConstraint = ( qualifiedName <~ '<' ) ~ qualifiedName.+('<') ^^ {
    case head ~ tail => ( head :: tail ).reverse
  }

  lazy val descendingConstraint = ( qualifiedName <~ '>' ) ~ qualifiedName.+('>') ^^ {
    case head ~ tail => head :: tail
  }

  lazy val formalParameters = '(' ~> formalParameter.*(',') <~ ')'

  lazy val formalParameter = modifiers ~ parameterType ~ dots ~ identifier ~ emptyBrackets ~ ( '=' ~> expression ).? ^^ {
    case mods ~ pt ~ ds ~ name ~ dim ~ init => FormalParameter(mods, pt, ds, name, dim, init)
  }

  lazy val declarator = identifier ~ emptyBrackets ~ ( '=' ~> expression ).? ^^ {
    case name ~ dim ~ init => VariableDeclarator(name, dim, init)
  }

  lazy val syntaxElement = operatorName | regexName | optionalOperand | repetition0 | repetition1 | operand | metaValueRef | andPredicate | notPredicate

  lazy val operatorName = elem[StrLiteral].^ ^^ { lit => OperatorName(lit.value) }
  lazy val regexName = (underscore ~> elem('%')).^ ~> elem[StrLiteral].^ ^^ { lit => RegexName(lit.value) }

  lazy val optionalOperand = (underscore ~> elem('?')).^ ~> priority ^^ OptionalOperand
  lazy val repetition0 = (underscore ~> elem('*')).^ ~> priority ^^ Repetition0
  lazy val repetition1 = (underscore ~> elem('+')).^ ~> priority ^^ Repetition1
  lazy val operand = underscore.^ ~> priority ^^ Operand

  lazy val metaValueRef = identifier ~ priority ^^ {
    case id ~ pri => MetaValueRef(id, pri)
  }

  lazy val andPredicate = ('&' ~> typeName) ~ priority ^^ {
    case t ~ p => AndPredicate(t, p)
  }
  lazy val notPredicate = ('!' ~> typeName) ~ priority ^^ {
    case t ~ p => NotPredicate(t, p)
  }

  lazy val underscore = elem(Identifier("_"))

  lazy val dots = (elem('.') ~> elem('.') ~> elem('.')).?.map(_.nonEmpty).^

  lazy val priority = ( '[' ~> qualifiedName <~ ']' ).?

  lazy val emptyBrackets = ('[' ~> ']').* ^^ { _.size }

  lazy val clause = throwsClause | activatesClause | deactivatesClause | requiresClause

  lazy val throwsClause = "throws" ~> typeName.+(',') ^^ ThrowsClause

  lazy val activatesClause = "activates" ~> typeName.+(',') ^^ ActivatesClause

  lazy val deactivatesClause = "deactivates" ~> typeName.+(',') ^^ DeactivatesClause

  lazy val requiresClause = "requires" ~> typeName.+(',') ^^ RequiresClause

  lazy val methodBody = block ^^ { Some(_) } | ';' ^^^ None

  case class ClassMemberParsers (className: String) {
    lazy val classBody = '{' ~> classMember.* <~ '}'

    lazy val enumBody = '{' ~> ( enumConstants ~ enumMembers ) <~ '}'

    lazy val dslBody = '{' ~> dslMember.* <~ '}'

    lazy val classMember: HParser[ClassMember] =
      instanceInitializer | staticInitializer | constructorDeclaration | methodDeclaration | fieldDeclaration | moduleDeclaration

    lazy val enumMembers = ( ';' ~> classMember.* ).? ^^ { _.getOrElse(Nil) }

    lazy val enumConstants = enumConstant.*(',') <~ ','.?

    lazy val enumConstant = annotation.* ~ identifier ~ ( '(' ~> expression.*(',') <~ ')' ).? ~ classBody.? ^^ {
      case as ~ name ~ args ~ body => EnumConstant(as, name, args.getOrElse(Nil), body.getOrElse(Nil))
    }

    lazy val dslMember = prioritiesDeclaration | constructorDeclaration | operatorDeclaration | fieldDeclaration

    lazy val constructorDeclaration = modifiers ~ metaParameters ~ className ~ formalParameters ~ clause.* ~ block ^^ {
      case mods ~ mps ~ _ ~ params ~ clauses ~ body => ConstructorDeclaration(mods, mps, params, clauses, body)
    }
  }

  /* types */

  lazy val parameterType: HParser[ParameterType] = contextualType | typeName

  lazy val contextualType = ( typeName <~ ( elem('~') ~> elem('>') ).^ ) ~ parameterType ^^ {
    case ct ~ pt => ContextualType(ct, pt)
  }

  lazy val returnBound: HParser[TypeName] = '^' ~> typeName

  lazy val typeName: HParser[TypeName] = className ~ typeArguments ~ emptyBrackets ^^ {
    case name ~ args ~ dim => TypeName(name, args, dim)
  }

  lazy val className: HParser[QualifiedName] = ( "class".! ~> identifier ).+('.') ^^ QualifiedName

  lazy val metaParameters = ( '<' ~> metaParameter.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }

  lazy val metaParameter = metaValueParameter | typeParameter

  lazy val metaValueParameter = identifier ~ ( ':' ~> typeName ) ^^ {
    case name ~ metaType => MetaValueParameter(name, metaType)
  }

  lazy val typeParameter = identifier ~ ( "extends" ~> typeName.+('&') ).? ^^ {
    case name ~ bounds => TypeParameter(name, bounds.getOrElse(Nil))
  }

  lazy val typeArguments = ( '<' ~> typeArgument.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }

  lazy val typeArgument = wildcardType | typeName

  lazy val wildcardType = wildcardExtends | wildcardSuper | unboundWildcard

  lazy val wildcardExtends = '?' ~> "extends" ~> typeName ^^ { bound => UpperBoundWildcardType(bound) }

  lazy val wildcardSuper = '?' ~> "super" ~> typeName ^^ { bound => LowerBoundWildcardType(bound) }

  lazy val unboundWildcard = '?' ^^^ UnboundWildcardType


  /* modifiers and annotations */

  lazy val modifiers = (annotation | modifier).*

  lazy val modifier: HParser[Modifier] =
    "public" ^^^ PublicModifier | "private" ^^^ PrivateModifier | "protected" ^^^ ProtectedModifier |
      "static" ^^^ StaticModifier | "final" ^^^ FinalModifier | "synchronized" ^^^ SynchronizedModifier |
      "volatile" ^^^ VolatileModifier | "transient" ^^^ TransientModifier | "native" ^^^ NativeModifier |
      "abstract" ^^^ AbstractModifier | "strictfp" ^^^ StrictFPModifier | "pure" ^^^ PureModifier |
      "literal" ^^^ LiteralModifier | "exact" ^^^ ExactModifier

  lazy val annotation = fullAnnotation | singleElementAnnotation | markerAnnotation

  lazy val fullAnnotation = annotationName ~ ( '(' ~> fullAnnotationArgument.*(',') <~ ')' ) ^^ {
    case name ~ args => FullAnnotation(name, args.toMap)
  }

  lazy val fullAnnotationArgument = ( identifier <~ '=' ) ~ annotationElement ^^ {
    case name ~ arg => (name, arg)
  }

  lazy val singleElementAnnotation = annotationName ~ ( '(' ~> annotationElement <~ ')' ) ^^ {
    case name ~ elem => SingleElementAnnotation(name, elem)
  }

  lazy val markerAnnotation = annotationName ^^ MarkerAnnotation

  lazy val annotationName = '@' ~> "interface".! ~> qualifiedName

  lazy val annotationElement: HParser[AnnotationElement] = annotation | arrayOfAnnotationElement | annotationExpression | enumConstant

  lazy val arrayOfAnnotationElement = '{' ~> annotationElement.*(',') <~ ','.? <~ '}' ^^ ArrayOfAnnotationElement


  /* body code snippet */

  lazy val block = positioned(blockCode.^ ^^ {
    case code => BlockSnippet(code.map(_.raw).mkString)
  })

  lazy val expression = positioned(expressionCode.*.^ ^^ {
    case code => ExpressionSnippet(code.flatten.map(_.raw).mkString)
  })

  lazy val expressionCode = blockCode | parenthesizedCode | without('(', ')', '{', '}', ',', ';').+

  lazy val parenthesizedCode: LParser[List[DToken]] = elem('(') ~> argumentCode.* <~ elem(')') ^^ { Symbol('(') +: _.flatten :+ Symbol(')') }

  lazy val argumentCode = parenthesizedCode | without('(', ')').+

  lazy val blockCode: LParser[List[DToken]] = elem('{') ~> blockStatementCode.* <~ elem('}') ^^ { Symbol('{') +: _.flatten :+ Symbol('}') }

  lazy val blockStatementCode = blockCode | without('{', '}').+

  /* annotation expression */

  lazy val annotationExpression = stringLiteral | classLiteral

  lazy val stringLiteral = elem[StrLiteral].^ ^^ { s => StringLiteralExpression(s.value) }

  lazy val classLiteral = typeName ^^ ClassLiteralExpression

  lazy val enumConstant = qualifiedName ^^ { name => EnumConstantElement(name.names) }

  /* primary */

  lazy val qualifiedName = identifier.+('.') ^^ QualifiedName

  lazy val identifier = accept("identifier", { case Identifier(id) => id }).^

  lazy val delimiter = elem[Whitespace].*

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

  lazy val identifier = elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).* ^^ {
    case s ~ ps => Identifier(s +: ps.mkString)
  }

  lazy val space = elem("white space", Character.isWhitespace) ^^ { Whitespace(_) }

  lazy val charLiteral = '\'' ~> (escapeSequence | except('\'')) <~ '\'' ^^ { CharLiteral(_) }

  lazy val stringLiteral = '\"' ~> (escapeSequence | except('\"')).* <~ '\"' ^^ { cs => StrLiteral(cs.mkString) }

  lazy val symbol = except() ^^ { Symbol(_) }

  lazy val escapeSequence = octalEscape | escape('b', '\b') | escape('f', '\f') | escape('n', '\n') | escape('r', '\r') | escape('t', '\t') | escape('\'', '\'') | escape('\"', '\"') | escape('\\', '\\')

  def escape (symbol: Char, character: Char) = '\\' ~> symbol ^^^ character

  lazy val octalEscape = octalEscape3 | octalEscape2 | octalEscape1

  lazy val octalEscape1 = '\\' ~> octalDigit ^^ { _.toChar }
  lazy val octalEscape2 = '\\' ~> ( octalDigit ~ octalDigit ) ^^ { case a ~ b => ((a << 3) + b).toChar }
  lazy val octalEscape3 = '\\' ~> ( quaternaryDigit ~ octalDigit ~ octalDigit ) ^^ { case a ~ b ~ c => ((a << 6) + (b << 3) + c).toChar }

  lazy val quaternaryDigit = elem("quaternary digit", { ch => '0' <= ch && ch <= '3' }) ^^ { Character.digit(_, 4) }
  lazy val octalDigit = elem("octal digit", { ch => '0' <= ch && ch <= '7' }) ^^ { Character.digit(_, 8) }

  lazy val lineComment = '/' ~> '/' ~> except('\n').*
  lazy val blockComment = '/' ~> '*' ~> ( except('*') | '*' <~ not('/') ).* <~ '*' <~ '/'

  override def errorToken(msg: String): DToken = ErrorToken(msg)

  def except(cs: Char*) = elem("", c => ! cs.contains(c) && c != EofCh)
}

object DeclarationPreprocessor extends Scanners {
  override type Token = Char

  lazy val token: Parser[Char] = unicodeEscape | elem("any", _ => true)

  lazy val unicodeEscape = '\\' ~> elem('u').+ ~> ( hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ) ^^ {
    case a ~ b ~ c ~ d => ((a << 12) + (b << 8) + (c << 4) + d).toChar
  }

  lazy val hexDigit = elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ { Character.digit(_, 16) }

  val whitespace: Parser[Unit] = success(())

  def errorToken(msg: String): Char = EofCh
}
