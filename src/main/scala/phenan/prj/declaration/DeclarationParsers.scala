package phenan.prj.declaration

import phenan.prj.combinator.TwoLevelParsers

import scala.language.implicitConversions
import scala.util.parsing.combinator.lexical.Scanners
import scala.util.parsing.input.CharArrayReader.EofCh

import scalaz.Memo._

object DeclarationParsers extends TwoLevelParsers {
  override type Elem = DToken

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

  lazy val importStaticStar = "import" ~> "static" ~> qualifiedName <~ '.' <~ '*' <~ ';' ^^ ImportStaticStarDeclaration

  lazy val importDSLs = importDSLs_ascending | importDSLs_descending

  lazy val importDSLs_ascending = "import" ~> "dsl" ~> qualifiedName.+('<') <~ ';' ^^ { dsls => ImportDSLsDeclaration(dsls) }

  lazy val importDSLs_descending = "import" ~> "dsl" ~> qualifiedName.+('>') <~ ';' ^^ { dsls => ImportDSLsDeclaration(dsls.reverse) }

  lazy val importClass = "import" ~> qualifiedName <~ ';' ^^ ClassImportDeclaration

  lazy val importPackage = "import" ~> qualifiedName <~ '.' <~ '*' <~ ';' ^^ PackageImportDeclaration


  /* module declarations */

  lazy val moduleDeclaration: HParser[ModuleDeclaration] = classDeclaration | enumDeclaration | interfaceDeclaration | annotationDeclaration | dslDeclaration

  lazy val classDeclaration = modifiers ~ ( "class" ~> identifier ) ~ typeParameters ~ ( "extends" ~> typeName ).? ~ implementsInterfaces >> {
    case mods ~ name ~ tps ~ sup ~ ifs => ClassMemberParsers(name).classBody ^^ { body => ClassDeclaration(mods, name, tps, sup, ifs, body) }
  }

  lazy val enumDeclaration = modifiers ~ ( "enum" ~> identifier ) ~ implementsInterfaces >> {
    case mods ~ name ~ ifs => ClassMemberParsers(name).enumBody ^^ {
      case cns ~ members => EnumDeclaration(mods, name, ifs, cns, members)
    }
  }

  lazy val interfaceDeclaration = modifiers ~ ( "interface" ~> identifier ) ~ typeParameters ~ extendsInterfaces ~ ( '{' ~> interfaceMember.* <~ '}' ) ^^ {
    case mods ~ name ~ tps ~ ifs ~ body => InterfaceDeclaration(mods, name, tps, ifs, body)
  }

  lazy val annotationDeclaration = modifiers ~ ( '@' ~> "interface" ~> identifier ) ~ ( '{' ~> annotationMember.* <~ '}' ) ^^ {
    case mods ~ name ~ body => AnnotationDeclaration(mods, name, body)
  }

  lazy val dslDeclaration: HParser[DSLDeclaration] = ???

  lazy val implementsInterfaces = ( "implements" ~> typeName.+(',')).? ^^ { _.getOrElse(Nil) }

  lazy val extendsInterfaces = ( "extends" ~> typeName.+(',')).? ^^ { _.getOrElse(Nil) }


  /* members*/

  lazy val interfaceMember: HParser[ClassMember] = methodDeclaration | fieldDeclaration | moduleDeclaration

  lazy val annotationMember: HParser[AnnotationMember] = annotationElementDeclaration | fieldDeclaration | moduleDeclaration

  lazy val instanceInitializer = block ^^ InstanceInitializer

  lazy val staticInitializer = "static" ~> block ^^ StaticInitializer

  lazy val methodDeclaration = modifiers ~ typeParameters ~ typeName ~ identifier ~ formalParameters ~ throwsClause ~ methodBody ^^ {
    case mods ~ tps ~ ret ~ name ~ params ~ throws ~ body => MethodDeclaration(mods, tps, ret, name, params, throws, body)
  }

  lazy val fieldDeclaration = modifiers ~ typeName ~ declarator.+(',') <~ ';' ^^ {
    case mods ~ t ~ ds => FieldDeclaration(mods, t, ds)
  }

  lazy val annotationElementDeclaration = modifiers ~ typeName ~ ( identifier <~ '(' <~ ')' ) ~ emptyBrackets ~ ( "default" ~> annotationElement ).? <~ ';' ^^ {
    case mods ~ tn ~ name ~ dim ~ default => AnnotationElementDeclaration(mods, tn, name, dim, default)
  }

  lazy val formalParameters: HParser[List[FormalParameter]] = '(' ~> formalParameter.*(',') <~ ')'

  lazy val formalParameter: HParser[FormalParameter] = modifiers ~ parameterType ~ dots ~ identifier ~ emptyBrackets ~ ( '=' ~> expression ).? ^^ {
    case mods ~ pt ~ varArgs ~ name ~ dim ~ init => FormalParameter(mods, pt, varArgs, name, dim, init)
  }

  lazy val declarator: HParser[VariableDeclarator] = identifier ~ emptyBrackets ~ ( '=' ~> expression ).? ^^ {
    case name ~ dim ~ init => VariableDeclarator(name, dim, init)
  }

  lazy val dots = (elem('.') ~> elem('.') ~> elem('.')).?.map(_.nonEmpty).^

  lazy val emptyBrackets = ('[' ~> ']').* ^^ { _.size }

  lazy val throwsClause: HParser[List[TypeName]] = "throws" ~> typeName.+(',')

  lazy val methodBody = block ^^ { Some(_) } | ';' ^^^ None

  case class ClassMemberParsers (className: String) {

    lazy val member: HParser[ClassMember] =
      instanceInitializer | staticInitializer | constructorDeclaration | methodDeclaration | fieldDeclaration | moduleDeclaration

    lazy val classBody = '{' ~> member.* <~ '}'

    lazy val enumBody = '{' ~> ( enumConstants ~ enumMembers ) <~ '}'

    lazy val enumMembers = ( ';' ~> member.* ).? ^^ { _.getOrElse(Nil) }

    lazy val enumConstants = enumConstant.*(',') <~ ','.?

    lazy val enumConstant = annotation.* ~ identifier ~ ( '(' ~> expression.*(',') <~ ')' ).? ~ classBody.? ^^ {
      case as ~ name ~ args ~ body => EnumConstant(as, name, args.getOrElse(Nil), body.getOrElse(Nil))
    }

    lazy val constructorDeclaration = modifiers ~ typeParameters ~ className ~ formalParameters ~ throwsClause ~ block ^^ {
      case mods ~ tps ~ _ ~ params ~ throws ~ body => ConstructorDeclaration(mods, tps, params, throws, body)
    }
  }

  /* types */

  lazy val parameterType: HParser[ParameterType] = contextualType | typeName

  lazy val contextualType = ( typeName <~ ( elem('~') ~> elem('>') ).^ ) ~ parameterType ^^ {
    case ct ~ pt => ContextualType(ct, pt)
  }

  lazy val typeName: HParser[TypeName] = qualifiedName ~ typeArguments ~ emptyBrackets ^^ {
    case name ~ args ~ dim => TypeName(name, args, dim)
  }

  lazy val typeParameters = ( '<' ~> typeParameter.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }

  lazy val typeParameter = identifier ~ ( "extends" ~> typeName.+('&') ).? ^^ {
    case name ~ bounds => TypeParameter(name, bounds.getOrElse(Nil))
  }

  lazy val typeArguments = ( '<' ~> typeArgument.+(',') <~ '>' ).? ^^ { _.getOrElse(Nil) }

  lazy val typeArgument = wildcardType | typeName

  lazy val wildcardType = wildcardExtends | wildcardSuper | unboundWildcard

  lazy val wildcardExtends = '?' ~> "extends" ~> typeName ^^ { bound => WildcardType(Some(bound), None) }

  lazy val wildcardSuper = '?' ~> "super" ~> typeName ^^ { bound => WildcardType(None, Some(bound)) }

  lazy val unboundWildcard = '?' ^^^ WildcardType(None, None)


  /* modifiers and annotations */

  lazy val modifiers = (annotation | modifier).*

  lazy val modifier: HParser[Modifier] =
    "public" ^^^ PublicModifier | "private" ^^^ PrivateModifier | "protected" ^^^ ProtectedModifier |
      "static" ^^^ StaticModifier | "final" ^^^ FinalModifier | "synchronized" ^^^ SynchronizedModifier |
      "volatile" ^^^ VolatileModifier | "transient" ^^^ TransientModifier | "native" ^^^ NativeModifier |
      "abstract" ^^^ AbstractModifier | "strictfp" ^^^ StrictFPModifier | "pure" ^^^ PureModifier

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

  lazy val annotationElement: HParser[AnnotationElement] = annotation | arrayOfAnnotationElement | expression

  lazy val arrayOfAnnotationElement = '{' ~> annotationElement.*(',') <~ ','.? <~ '}' ^^ ArrayOfAnnotationElement


  /* body code snippet */

  lazy val block = (position ~ blockCode).^ ^^ {
    case pos ~ code => BlockSnippet(code.map(_.raw).mkString, pos)
  }

  lazy val expression = (position ~ expressionCode).^ ^^ {
    case pos ~ code => ExpressionSnippet(code.map(_.raw).mkString, pos)
  }

  lazy val expressionCode = blockCode | parenthesizedCode | without(')', '}', ',', ';').*

  lazy val parenthesizedCode: LParser[List[DToken]] = elem('(') ~> argumentCode.* <~ elem(')') ^^ { Symbol('(') +: _.flatten :+ Symbol(')') }

  lazy val argumentCode = parenthesizedCode | without(')').*

  lazy val blockCode: LParser[List[DToken]] = elem('{') ~> blockStatementCode.* <~ elem('}') ^^ { Symbol('{') +: _.flatten :+ Symbol('}')}

  lazy val blockStatementCode = blockCode | without('}').*


  /* primary */

  lazy val qualifiedName = identifier.+('.') ^^ QualifiedName

  lazy val identifier = accept("identifier", { case Identifier(id) => id }).^

  lazy val delimiter = elem[Whitespace]

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

  lazy val quaternaryDigit = elem("hex digit", { ch => '0' <= ch && ch <= '3' }) ^^ { Character.digit(_, 4) }
  lazy val octalDigit = elem("hex digit", { ch => '0' <= ch && ch <= '7' }) ^^ { Character.digit(_, 8) }

  lazy val lineComment = '/' ~> '/' ~> except('\n').*
  lazy val blockComment = '/' ~> '*' ~> ( except('*') | '*' <~ not('/') ).* <~ '*' <~ '/'

  override def errorToken(msg: String): DToken = ErrorToken(msg)

  def except(cs: Char*) = elem("", c => cs.forall(c != _) && c != EofCh)
}

object DeclarationPreprocessor extends Scanners {
  override type Token = Char

  lazy val token: Parser[Char] = unicodeEscape | elem("any", _ => true)

  lazy val unicodeEscape = '\\' ~> elem('u').+ ~> ( hexDigit ~ hexDigit ~ hexDigit ~ hexDigit ) ^^ {
    case a ~ b ~ c ~ d => ((a << 12) + (b << 8) + (c << 4) + d).toChar
  }

  lazy val hexDigit = elem("hex digit", { ch => '0' <= ch && ch <= '9' || 'a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F' }) ^^ { Character.digit(_, 16) }

  val whitespace: Parser[Unit] = success(())

  def errorToken(msg: String): Char = throw new UnsupportedOperationException
}

sealed trait DToken {
  def raw: String
}

class Identifier private (val id: String) extends DToken {
  def raw = id
}

class Symbol private (val symbol: Char) extends DToken {
  def raw = symbol.toString
}

class Whitespace private (ws: Char) extends DToken {
  def raw = ws.toString
}

class CharLiteral private (literal: Char) extends DToken {
  def raw = '\'' + LiteralUtil.escape(literal) + '\''
}

class StrLiteral private (literal: String) extends DToken {
  def raw = '\"' + literal.flatMap(LiteralUtil.escape) + '\"'
}

class ErrorToken private (msg: String) extends DToken {
  def raw = "<error>"
}

object LiteralUtil {
  def escape (c: Char): String = c match {
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case '\'' => "\\\'"
    case '\"' => "\\\""
    case '\\' => "\\\\"
    case x    => x.toString
  }
}

object Identifier {
  def apply (id: String): Identifier = get(id)
  def unapply (id: Identifier): Option[String] = Some(id.id)
  private val get: String => Identifier = mutableHashMapMemo(new Identifier(_))
}

object Symbol {
  def apply (s: Char): Symbol = get(s)
  def unapply (s: Symbol): Option[Char] = Some(s.symbol)
  private val get: Char => Symbol = mutableHashMapMemo(new Symbol(_))
}

object Whitespace {
  def apply (ws: Char): Whitespace = get(ws)
  private val get: Char => Whitespace = mutableHashMapMemo(new Whitespace(_))
}

object CharLiteral {
  def apply (lit: Char): CharLiteral = new CharLiteral(lit)
}

object StrLiteral {
  def apply (lit: String): StrLiteral = new StrLiteral(lit)
}

object ErrorToken {
  def apply (msg: String): ErrorToken = new ErrorToken(msg)
}
