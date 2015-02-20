package phenan.prj.decl

import java.io._

import phenan.prj.exception.ParseException
import phenan.prj.state.JState

import scala.util._

object DeclarationParser {
  def apply (reader: Reader)(implicit state: JState): Try[DeclarationParser] = {
    SourceReader(reader).map(new DeclarationParser(_))
  }
}

class DeclarationParser private (private val reader: SourceReader)(implicit state: JState) {
  lazy val parseAll: CompilationUnit = parseCompilationUnit

  /* BNF-like notation
   *
   * # indicates the current position
   * E.? indicates E is optional
   * E.* indicates zero or more times repetition of E
   * E.+ indicates zero or more times repetition of E
   * E.*A* indicates zero or more times repetition of E with separator A
   */

  /* CompilationUnit
   *  : # Header List[ModuleDeclaration]
   */
  private def parseCompilationUnit: CompilationUnit = {
    val header  = parseHeader
    val modules = parseModuleDeclarations(Nil)
    CompilationUnit(header, modules)
  }

  /* Header
   *  : # Option[PackageDeclaration] List[ImportDeclaration]
   */
  private def parseHeader: Header = {
    val pack    = parsePackageDeclarationOpt
    val imports = parseImportDeclarations(Nil)
    Header(pack, imports)
  }

  /* Option[PackageDeclaration]
   *  : # PackageDeclaration.?
   */
  private def parsePackageDeclarationOpt: Option[PackageDeclaration] = {
    if (read("package")) parsePackageDeclaration match {
      case Success(pack) => Some(pack)
      case Failure(e)    =>
        state.error("parse error : invalid package declaration", e)
        reader.skipUntil (_ is ';')
        None
    }
    else None
  }

  /* PackageDeclaration
   *  : "package" # QualifiedName ';'
   */
  private def parsePackageDeclaration: Try[PackageDeclaration] = for {
    name <- parseQualifiedName
    _    <- parseToken(';')
  } yield PackageDeclaration(name)

  /* List[ImportDeclaration]
   *  : # ImportDeclaration.*
   */
  private def parseImportDeclarations (imports: List[ImportDeclaration]): List[ImportDeclaration] = {
    if (read("import")) parseImportDeclaration match {
      case Success(d) => parseImportDeclarations(imports :+ d)
      case Failure(e) =>
        state.error("parse error : invalid import declaration", e)
        reader.skipUntil (_ is ';')
        parseImportDeclarations(imports)
    }
    else imports
  }

  /* ImportDeclaration
   *  : StaticImportDeclaration   ; "import" # "static" QualifiedName [ '.' '*' ] ';'
   *  | DSLImportDeclaration      ; "import" # "dsl" QualifiedName DSLPrecedence ';'
   *  | ClassImportDeclaration    ; "import" # QualifiedName [ '.' '*' ] ';'
   */
  private def parseImportDeclaration: Try[ImportDeclaration] = {
    if (read("static")) parseStaticImportDeclaration
    else if (read("dsl")) parseDSLImportDeclaration
    else parseClassImportDeclaration
  }

  /* StaticImportDeclaration
   *  : "import" "static" # QualifiedName [ '.' '*' ] ';'
   */
  private def parseStaticImportDeclaration: Try[StaticImportDeclaration] = parseQualifiedName.flatMap { name =>
    if (reader.look(0).is('.') && reader.look(1).is('*') && reader.look(2).is(';')) {
      reader.next(2)
      Success(AllStaticMembersImportDeclaration(name))
    }
    else if (read(';')) Success(StaticMemberImportDeclaration(name))
    else Failure(parseError("'.*' or ';'"))
  }

  /* DSLImportDeclaration
   *  : "import" "dsl" # QualifiedName DSLPrecedence.? ';'
   */
  private def parseDSLImportDeclaration: Try[DSLImportDeclaration] = parseQualifiedName.flatMap { name =>
    if (read(';')) Success(DSLImportDeclaration(name, None))
    else for {
      precedence <- parseDSLPrecedence
      _          <- parseToken(';')
    } yield DSLImportDeclaration(name, Some(precedence))
  }

  /* DSLPrecedence
   *  : AscendingDSLPrecedence     ; # ( '<' QualifiedName ).+
   *  | DescendingDSLPrecedence    ; # ( '>' QualifiedName ).+
   */
  private def parseDSLPrecedence: Try[DSLPrecedence] = {
    if (read('<')) parseAscendingDSLPrecedence(Nil)
    else if (read('>')) parseDescendingDSLPrecedence(Nil)
    else Failure(parseError("'<' or '>' or ';'"))
  }

  /* AscendingDSLPrecedence
   *  : '<' # QualifiedName ( '<' QualifiedName ).*
   */
  private def parseAscendingDSLPrecedence(names: List[QualifiedName]): Try[AscendingDSLPrecedence] = parseQualifiedName match {
    case Success(name) =>
      if (read('<')) parseAscendingDSLPrecedence(names :+ name)
      else Success(AscendingDSLPrecedence(names :+ name))
    case Failure(e) => Failure(e)
  }

  /* DescendingDSLPrecedence
   *  : '>' # QualifiedName ( '>' QualifiedName ).*
   */
  private def parseDescendingDSLPrecedence(names: List[QualifiedName]): Try[DescendingDSLPrecedence] = parseQualifiedName match {
    case Success(name) =>
      if (read('>')) parseDescendingDSLPrecedence(names :+ name)
      else Success(DescendingDSLPrecedence(names :+ name))
    case Failure(e) => Failure(e)
  }

  /* ClassImportDeclaration
   *  : "import" # QualifiedName [ '.' '*' ] ';'
   */
  private def parseClassImportDeclaration: Try[ClassImportDeclaration] = parseQualifiedName.flatMap { name =>
    if (reader.look(0).is('.') && reader.look(1).is('*') && reader.look(2).is(';')) {
      reader.next(2)
      Success(PackageImportDeclaration(name))
    }
    else if (read(';')) Success(SingleClassImportDeclaration(name))
    else Failure(parseError("'.*' or ';'"))
  }

  /* List[ModuleDeclaration]
   *  : # ModuleDeclaration.*
   */
  private def parseModuleDeclarations (modules: List[ModuleDeclaration]):  List[ModuleDeclaration] = {
    val pos = reader.position
    if (reader.eof) modules
    else parseModuleDeclaration match {
      case Success(m) => parseModuleDeclarations(modules :+ m)
      case Failure(e) =>
        state.error("parse error : invalid module declaration", e)
        reader.skipBlock(pos)
        parseModuleDeclarations(modules)
    }
  }

  /* ModuleDeclaration
   *  : ClassDeclaration         ; # List[ModuleModifier] "class" Identifier List[TypeParameter] ExtendsClass ImplementsInterfaces ClassBody
   *  | EnumDeclaration          ; # List[ModuleModifier] "enum" Identifier ImplementsInterfaces EnumBody
   *  | InterfaceDeclaration     ; # List[ModuleModifier] "interface" Identifier List[TypeParameter] ExtendsInterfaces.? InterfaceBody
   *  | AnnotationDeclaration    ; # List[ModuleModifier] '@' "interface" Identifier AnnotationBody
   *  | DSLDeclaration           ; # List[ModuleModifier] "dsl" Identifier DependsDSLs DSLBody
   */
  private def parseModuleDeclaration: Try[ModuleDeclaration] = parseModuleModifiers.flatMap { modifiers =>
    if (read("class")) parseClassDeclaration(modifiers)
    else ???
  }

  /* List[ModuleModifier]
   *  : ( Annotation | PublicModifier | PrivateModifier | ProtectedModifier
   *    | StaticModifier | FinalModifier | AbstractModifier | StrictFPModifier
   *    | AutoCloseModifier | PropagateModifier | PureModifier ).*
   */
  private def parseModuleModifiers: Try[List[ModuleModifier]] = parseModuleModifiers(Nil)

  private def parseModuleModifiers (list: List[ModuleModifier]): Try[List[ModuleModifier]] = reader.head match {
    case SymbolToken('@', _) =>
      reader.next  // '@'
      parseAnnotation match {
        case Success(ann) => parseModuleModifiers(list :+ ann)
        case Failure(e) => Failure(e)
      }
    case IdentifierToken(id, _) if moduleModifiers.contains(id) =>
      reader.next  // id
      parseModuleModifiers(list :+ moduleModifiers(id))
    case _ => Success(list)
  }

  private val moduleModifiers: Map[String, ModuleModifier] = Map(
    "public" -> PublicModifier, "private" -> PrivateModifier, "protected" -> ProtectedModifier,
    "static" -> StaticModifier, "final" -> FinalModifier, "abstract" -> AbstractModifier,
    "strictfp" -> StrictFPModifier, "autoclose" -> AutoCloseModifier, "propagate" -> PropagateModifier, "pure" -> PureModifier
  )

  /* Annotation
   *  : FullAnnotation             ; '@' # QualifiedName '(' ( Identifier '=' AnnotationElement ).*','*  ')'
   *  | SingleElementAnnotation    ; '@' # QualifiedName '(' AnnotationElement ')'
   *  | MarkerAnnotation           ; '@' # QualifiedName
   */
  private def parseAnnotation: Try[Annotation] = parseQualifiedName.flatMap { name =>
    if (read('(')) reader.look(0) match {
      case IdentifierToken(_, _) if reader.look(1) is '=' => parseFullAnnotation(name, Map.empty)
      case SymbolToken(')', _) => Success(FullAnnotation(name, Map.empty))
      case _ => for {
        arg <- parseAnnotationElement
        _   <- parseToken(')')
      } yield SingleElementAnnotation(name, arg)
    }
    else Success(MarkerAnnotation(name))
  }

  /* FullAnnotation
   *  : '@' QualifiedName '(' ( # Identifier '=' AnnotationElement ).*','* ')'
   */
  private def parseFullAnnotation(name: QualifiedName, args: Map[String, AnnotationElement]): Try[FullAnnotation] = reader.next match {
    case IdentifierToken(id, _) =>
      if (read('=')) parseAnnotationElement match {
        case Success(arg) =>
          if (read(',')) parseFullAnnotation(name, args + (id -> arg))
          else if (read(')')) Success(FullAnnotation(name, args + (id -> arg)))
          else Failure(parseError("',' or ')'"))
        case Failure(e) => Failure(e)
      }
      else Failure(parseError("'='"))
    case token => Failure(parseError("identifier", token))
  }

  /* AnnotationElement
   *  : Annotation
   *  | ArrayOfAnnotationElement
   *  | ExpressionSnippet
   */
  private def parseAnnotationElement: Try[AnnotationElement] = {
    if (read('@')) parseAnnotation
    else if (read('{')) parseArrayOfAnnotationElement
    else parseExpressionSnippet
  }

  /* ArrayOfAnnotationElement
   *  : '{' # ( AnnotationElement ( ',' AnnotationElement ).* ).? ','.? '}'
   */
  private def parseArrayOfAnnotationElement: Try[ArrayOfAnnotationElement] = {
    if (read('}')) Success(ArrayOfAnnotationElement(Nil))
    else if (read(',')) {
      if (read('}')) Success(ArrayOfAnnotationElement(Nil))
      else Failure(parseError("'}'"))
    }
    else parseAnnotationElement.flatMap(elem => parseArrayOfAnnotationElement(List(elem)))
  }

  /* ArrayOfAnnotationElement
   *  : '{' ( AnnotationElement # ( ',' AnnotationElement ).* ).? ','.? '}'
   */
  private def parseArrayOfAnnotationElement(array: List[AnnotationElement]): Try[ArrayOfAnnotationElement] = {
    if (read(',')) {
      if (read('}')) Success(ArrayOfAnnotationElement(array))
      else parseAnnotationElement match {
        case Success(elem) => parseArrayOfAnnotationElement(array :+ elem)
        case Failure(e)    => Failure(e)
      }
    }
    else if (read('}')) Success(ArrayOfAnnotationElement(array))
    else Failure(parseError("',' or '}'"))
  }

  /* ClassDeclaration
   *  : List[ModuleModifier] "class" # Identifier List[TypeParameter] ExtendsClass ImplementsInterfaces ClassBody
   */
  private def parseClassDeclaration (modifiers: List[ModuleModifier]): Try[ClassDeclaration] = for {
    name           <- parseIdentifier
    typeParameters <- parseTypeParameters
    extendsClass   <- parseExtendsClass
    interfaces     <- parseImplementsInterfaces
    members        <- parseClassBody
  } yield ClassDeclaration(modifiers, name, typeParameters, extendsClass, interfaces, members)

  /* List[TypeParameter]
   *  : # ( '<' TypeParameter ( ',' TypeParameter ).* '>' ).?
   */
  private def parseTypeParameters: Try[List[TypeParameter]] = {
    if (read('<')) parseTypeParameter.flatMap { param => parseTypeParameters(List(param)) }
    else Success(Nil)
  }

  /* List[TypeParameter]
   *  : # ( '<' TypeParameter # ( ',' TypeParameter ).* '>' ).?
   */
  private def parseTypeParameters (params: List[TypeParameter]): Try[List[TypeParameter]] = {
    if (read(',')) parseTypeParameter match {
      case Success(param) => parseTypeParameters(params :+ param)
      case Failure(e)     => Failure(e)
    }
    else Success(params)
  }

  /* TypeParameter
   *  : # Identifier ( "extends" ClassTypeName ( '&' ClassTypeName ).* ).?
   */
  private def parseTypeParameter: Try[TypeParameter] = parseIdentifier.flatMap { name =>
    if (read("extends")) parseClassTypeName.flatMap { bound => parseTypeParameter(name, List(bound)) }
    else Success(TypeParameter(name, Nil))
  }

  /* TypeParameter
   *  : Identifier ( "extends" ClassTypeName # ( '&' ClassTypeName ).* ).?
   */
  private def parseTypeParameter (name: String, bounds: List[ClassTypeName]): Try[TypeParameter] = {
    if (read('&')) parseClassTypeName match {
      case Success(bound) => parseTypeParameter(name, bounds :+ bound)
      case Failure(e)     => Failure(e)
    }
    else Success(TypeParameter(name, bounds))
  }

  /* ExtendsClass
   *  : ( "extends" ClassTypeName ).?
   */
  private def parseExtendsClass: Try[Option[ClassTypeName]] = {
    if (read("extends")) parseClassTypeName.map(Some(_))
    else Success(None)
  }

  /* ImplementsInterfaces
   *  : # ( "implements" ClassTypeName ( ',' ClassTypeName ).* ).?
   */
  private def parseImplementsInterfaces: Try[List[ClassTypeName]] = {
    if (read("implements")) parseClassTypeName.flatMap(i => parseImplementsInterfaces(List(i)))
    else Success(Nil)
  }

  /* ImplementsInterfaces
   *  : ( "implements" ClassTypeName # ( ',' ClassTypeName ).* ).?
   */
  private def parseImplementsInterfaces(interfaces: List[ClassTypeName]): Try[List[ClassTypeName]] = {
    if (read(',')) parseClassTypeName match {
      case Success(i) => parseImplementsInterfaces(interfaces :+ i)
      case Failure(e) => Failure(e)
    }
    else Success(interfaces)
  }

  /* ClassBody
   *  : # '{' ClassMember.* '}'
   */
  private def parseClassBody: Try[List[ClassMember]] = parseToken('{').flatMap(_ => parseClassBody(Nil))

  /* ClassBody
   *  : '{' # ClassMember.* '}'
   */
  private def parseClassBody(members: List[ClassMember]): Try[List[ClassMember]] = {
    if (read('}')) Success(members)
    else parseClassMember match {
      case Success(member) => parseClassBody(members :+ member)
      case Failure(e) => Failure(e)
    }
  }

  /* ClassMember
   *
   */
  private def parseClassMember: Try[ClassMember] = ???

  /* TypeName
   *  : # ClassTypeName ( '[' ']' ).*
   */
  private def parseTypeName: Try[TypeName] = parseClassTypeName.flatMap(parseTypeName)

  /* TypeName
   *  : ClassTypeName # ( '[' ']' ).*
   */
  private def parseTypeName (component: TypeName): Try[TypeName] = {
    if (read('[')) {
      if (read(']')) parseTypeName(ArrayTypeName(component))
      else Failure(parseError("']'"))
    }
    else Success(component)
  }

  /* ClassTypeName
   *  : # QualifiedName List[TypeArgument]
   */
  private def parseClassTypeName: Try[ClassTypeName] = for {
    name <- parseQualifiedName
    args <- parseTypeArguments
  } yield ClassTypeName(name, args)

  /* List[TypeArgument]
   *  : # ( '<' TypeArgument ( ',' TypeArgument ) '>' ).?
   */
  private def parseTypeArguments: Try[List[TypeArgument]] = {
    if (read('<')) parseTypeArgument.flatMap { arg => parseTypeArguments(List(arg)) }
    else Success(Nil)
  }

  /* List[TypeArgument]
   *  : ( '<' TypeArgument # ( ',' TypeArgument ) '>' ).?
   */
  private def parseTypeArguments (args: List[TypeArgument]): Try[List[TypeArgument]] = {
    if (read(',')) parseTypeArgument match {
      case Success(arg) => parseTypeArguments(args :+ arg)
      case Failure(e)   => Failure(e)
    }
    else if (read('>')) Success(args)
    else Failure(parseError("',' or '>'"))
  }

  /* TypeArgument
   *  : # WildcardType
   *  | # TypeName
   */
  private def parseTypeArgument: Try[TypeArgument] = {
    if (read('?')) parseWildcardType
    else parseTypeName
  }

  /* WildcardType
   *  : '?' #
   *  | '?' # "extends" TypeName
   *  | '?' # "super" TypeName
   */
  private def parseWildcardType: Try[WildcardType] = {
    if (read("extends")) parseTypeName.map(bound => WildcardType(Some(bound), None))
    else if (read("super")) parseTypeName.map(bound => WildcardType(None, Some(bound)))
    else Success(WildcardType(None, None))
  }

  /* QualifiedName
   *  : # Identifier ( '.' Identifier ).*
   */
  private def parseQualifiedName: Try[QualifiedName] = reader.next match {
    case IdentifierToken(id, _) => Success(parseQualifiedName(List(id)))
    case token => Failure(parseError("identifier", token))
  }

  /* QualifiedName
   *  : Identifier # ( '.' Identifier ).*
   */
  private def parseQualifiedName (names: List[String]): QualifiedName = {
    if (reader.look(0).is('.')) reader.look(1) match {
      case IdentifierToken(id, _) =>
        reader.next
        reader.next
        parseQualifiedName(names :+ id)
      case _ => QualifiedName(names)
    }
    else QualifiedName(names)
  }

  private def parseExpressionSnippet: Try[ExpressionSnippet] = reader.nextExpression.map(new ExpressionSnippet(_))

  private def parseIdentifier: Try[String] = reader.next match {
    case IdentifierToken(id, _) => Success(id)
    case token => Failure(parseError("identifier", token))
  }

  private def parseToken (sym: Char): Try[SymbolToken] = reader.next match {
    case s @ SymbolToken(ch, _) if ch == sym => Success(s)
    case t => Failure(parseError("'" + sym + "'", t))
  }

  private def read (id: String): Boolean = {
    if (reader.head is id) {
      reader.next
      true
    }
    else false
  }

  private def read (sym: Int): Boolean = {
    if (reader.head is sym) {
      reader.next
      true
    }
    else false
  }

  private def parseError (expected: String) = {
    ParseException("expected " + expected + ", but found " + reader.head)
  }

  private def parseError (expected: String, found: SourceToken) = {
    ParseException("expected " + expected + ", but found " + found)
  }
}

case class CompilationUnit (header: Header, modules: List[ModuleDeclaration])

case class Header (pack: Option[PackageDeclaration], imports: List[ImportDeclaration])

case class PackageDeclaration (name: QualifiedName)

sealed trait ImportDeclaration

sealed trait ClassImportDeclaration extends ImportDeclaration

case class SingleClassImportDeclaration (name: QualifiedName) extends ClassImportDeclaration

case class PackageImportDeclaration (name: QualifiedName) extends ClassImportDeclaration

sealed trait StaticImportDeclaration extends ImportDeclaration

case class StaticMemberImportDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class AllStaticMembersImportDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class DSLImportDeclaration (name: QualifiedName, precedence: Option[DSLPrecedence]) extends ImportDeclaration

sealed trait DSLPrecedence

case class AscendingDSLPrecedence (names: List[QualifiedName]) extends DSLPrecedence

case class DescendingDSLPrecedence (names: List[QualifiedName]) extends DSLPrecedence

sealed trait ModuleDeclaration

case class ClassDeclaration (modifiers: List[ModuleModifier], name: String, typeParameters: List[TypeParameter], superClass: Option[ClassTypeName], interfaces: List[ClassTypeName], members: List[ClassMember]) extends ModuleDeclaration

sealed trait PrjModifier

sealed trait ModuleModifier extends PrjModifier
sealed trait MemberModifier extends PrjModifier

case object PublicModifier extends ModuleModifier with MemberModifier
case object PrivateModifier extends ModuleModifier with MemberModifier
case object ProtectedModifier extends ModuleModifier with MemberModifier
case object StaticModifier extends ModuleModifier with MemberModifier
case object FinalModifier extends ModuleModifier with MemberModifier
case object SynchronizedModifier extends MemberModifier
case object VolatileModifier extends MemberModifier
case object TransientModifier extends MemberModifier
case object NativeModifier extends MemberModifier
case object AbstractModifier extends ModuleModifier with MemberModifier
case object StrictFPModifier extends ModuleModifier with MemberModifier

case object AutoCloseModifier extends ModuleModifier
case object PropagateModifier extends ModuleModifier
case object PureModifier extends ModuleModifier with MemberModifier

sealed trait AnnotationElement
sealed trait Annotation extends ModuleModifier with MemberModifier with AnnotationElement

case class FullAnnotation (name: QualifiedName, args: Map[String, AnnotationElement]) extends Annotation
case class SingleElementAnnotation (name: QualifiedName, arg: AnnotationElement) extends Annotation
case class MarkerAnnotation (name: QualifiedName) extends Annotation

case class ExpressionSnippet (snippet: Snippet) extends AnnotationElement
case class ArrayOfAnnotationElement (array: List[AnnotationElement]) extends AnnotationElement

sealed trait ClassMember

case class ConstructorDeclaration (modifiers: List[MemberModifier], typeParameters: List[TypeParameter], throws: List[TypeName], body: String)

case class TypeParameter (name: String, bounds: List[ClassTypeName])

sealed trait TypeArgument
sealed trait TypeName extends TypeArgument

case class ClassTypeName (name: QualifiedName, args: List[TypeArgument]) extends TypeName
case class ArrayTypeName (component: TypeName) extends TypeName
case class WildcardType (upper: Option[TypeName], lower: Option[TypeName]) extends TypeArgument

case class QualifiedName (names: List[String])




