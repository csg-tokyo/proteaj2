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
   *  : ClassDeclaration         ; # List[ModuleModifier] "class" Identifier List[TypeParameter] ExtendsClass.? Interfaces.? ClassBody
   *  | EnumDeclaration          ; # List[ModuleModifier] "enum" Identifier Interfaces.? EnumBody
   *  | InterfaceDeclaration     ; # List[ModuleModifier] "interface" Identifier List[TypeParameter] ExtendsInterfaces.? InterfaceBody
   *  | AnnotationDeclaration    ; # List[ModuleModifier] '@' "interface" Identifier AnnotationBody
   *  | DSLDeclaration           ; # List[ModuleModifier] "dsl" Identifier DependsDSLs DSLBody
   */
  private def parseModuleDeclaration: Try[ModuleDeclaration] = {
    ???
  }

  /* List[ModuleModifier]
   *  : ( AnnotationModifier | PublicModifier | PrivateModifier | ProtectedModifier
   *    | StaticModifier | FinalModifier | AbstractModifier | StrictFPModifier
   *    | AutoCloseModifier | PropagateModifier | PureModifier ).*
   */
  private def parseModuleModifiers: Try[List[ModuleModifier]] = parseModuleModifiers(Nil)

  private def parseModuleModifiers (list: List[ModuleModifier]): Try[List[ModuleModifier]] = reader.head match {
    case SymbolToken('@', _) =>
      reader.next  // '@'
      parseAnnotationModifier match {
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

  /* AnnotationModifier
   *  : FullAnnotation             ; '@' # QualifiedName '(' ( Identifier '=' Expression ).*','*  ')'
   *  | SingleElementAnnotation    ; '@' # QualifiedName '(' Expression ')'
   *  | MarkerAnnotation           ; '@' # QualifiedName
   */
  private def parseAnnotationModifier: Try[AnnotationModifier] = parseQualifiedName.flatMap { name =>
    if (read('(')) reader.look(0) match {
      case IdentifierToken(_, _) if reader.look(1) is '=' => parseFullAnnotation(name, Map.empty)
      case _ => reader.nextExpression.flatMap { arg =>
        if (read(')')) Success(SingleElementAnnotation(name, arg))
        else Failure(parseError("')'"))
      }
    }
    else Success(MarkerAnnotation(name))
  }

  /* FullAnnotation
   *  : '@' QualifiedName '(' ( # Identifier '=' Expression ).*','* ')'
   */
  private def parseFullAnnotation(name: QualifiedName, args: Map[String, String]): Try[FullAnnotation] = reader.next match {
    case IdentifierToken(id, _) =>
      if (read('=')) reader.nextExpression match {
        case Success(arg) =>
          if (read(',')) parseFullAnnotation(name, args + (id -> arg))
          else if (read(')')) Success(FullAnnotation(name, args + (id -> arg)))
          else Failure(parseError("',' or ')'"))
        case Failure(e) => Failure(e)
      }
      else Failure(parseError("'='"))
    case token => Failure(parseError("identifier", token))
  }

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

case class ClassDeclaration (modifiers: List[ModuleModifier], name: String) extends ModuleDeclaration

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

sealed trait AnnotationModifier extends ModuleModifier with MemberModifier

case class FullAnnotation (name: QualifiedName, args: Map[String, String]) extends AnnotationModifier
case class SingleElementAnnotation (name: QualifiedName, arg: String) extends AnnotationModifier
case class MarkerAnnotation (name: QualifiedName) extends AnnotationModifier

case class TypeParameter (name: String, bounds: List[ClassTypeName])

sealed trait TypeArgument
sealed trait TypeName extends TypeArgument

case class ClassTypeName (name: QualifiedName, args: List[TypeArgument]) extends TypeName
case class ArrayTypeName (component: TypeName) extends TypeName
case class WildcardType (upper: Option[TypeName], lower: Option[TypeName]) extends TypeArgument

case class QualifiedName (names: List[String])




