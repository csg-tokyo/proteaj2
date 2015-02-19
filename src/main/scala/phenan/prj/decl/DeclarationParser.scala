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
    val modules = parseListOfModuleDeclaration(Nil)
    CompilationUnit(header, modules)
  }

  /* Header
   *  : # Option[PackageDeclaration] List[ImportDeclaration]
   */
  private def parseHeader: Header = {
    val pack    = parseOptionalPackageDeclaration
    val imports = parseListOfImportDeclaration(Nil)
    Header(pack, imports)
  }

  /* Option[PackageDeclaration]
   *  : # PackageDeclaration.?
   */
  private def parseOptionalPackageDeclaration: Option[PackageDeclaration] = {
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
  private def parseListOfImportDeclaration (imports: List[ImportDeclaration]): List[ImportDeclaration] = {
    if (read("import")) parseImportDeclaration match {
      case Success(d) => parseListOfImportDeclaration(imports :+ d)
      case Failure(e) =>
        state.error("parse error : invalid import declaration", e)
        reader.skipUntil (_ is ';')
        parseListOfImportDeclaration(imports)
    }
    else imports
  }

  /* ImportDeclaration
   *  : StaticImportDeclaration   ; "import" # "static" QualifiedNameOrStar ';'
   *  | DSLImportDeclaration      ; "import" # "dsl" QualifiedName DSLPrecedence ';'
   *  | ClassImportDeclaration    ; "import" # QualifiedNameOrStar ';'
   */
  private def parseImportDeclaration: Try[ImportDeclaration] = {
    if (read("static")) parseStaticImportDeclaration
    else if (read("dsl")) parseDSLImportDeclaration
    else parseClassImportDeclaration
  }

  /* StaticImportDeclaration
   *  : StaticMemberImportDeclaration       ; "import" "static" # QualifiedName ';'
   *  | AllStaticMembersImportDeclaration   ; "import" "static" # QualifiedStar ';'
   */
  private def parseStaticImportDeclaration: Try[StaticImportDeclaration] = for {
    name <- parseQualifiedNameOrStar
    _    <- parseToken(';')
  } yield makeStaticImportDeclaration(name)

  private def makeStaticImportDeclaration (name: QualifiedNameOrStar): StaticImportDeclaration = name match {
    case q: QualifiedName => StaticMemberImportDeclaration(q)
    case q: QualifiedStar => AllStaticMembersImportDeclaration(q)
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
   *  : SingleClassImportDeclaration  ; "import" # QualifiedName ';'
   *  | PackageImportDeclaration      ; "import" # QualifiedStar ';'
   */
  private def parseClassImportDeclaration: Try[ClassImportDeclaration] = for {
    name <- parseQualifiedNameOrStar
    _    <- parseToken(';')
  } yield makeClassImportDeclaration(name)

  private def makeClassImportDeclaration (name: QualifiedNameOrStar): ClassImportDeclaration = name match {
    case q: QualifiedName => SingleClassImportDeclaration(q)
    case q: QualifiedStar => PackageImportDeclaration(q)
  }

  /* List[ModuleDeclaration]
   *  : # ModuleDeclaration.*
   */
  private def parseListOfModuleDeclaration (modules: List[ModuleDeclaration]):  List[ModuleDeclaration] = {
    val pos = reader.position
    if (reader.eof) modules
    else parseModuleDeclaration match {
      case Success(m) => parseListOfModuleDeclaration(modules :+ m)
      case Failure(e) =>
        state.error("parse error : invalid module declaration", e)
        reader.skipBlock(pos)
        parseListOfModuleDeclaration(modules)
    }
  }

  /* ModuleDeclaration
   *  : ClassDeclaration
   *  | InterfaceDeclaration
   *  | DSLDeclaration
   */
  private def parseModuleDeclaration: Try[ModuleDeclaration] = {
    ???
  }

  /* ClassModifier
   *  : ( AnnotationModifier | AccessModifier | StaticModifier | FinalModifier | AbstractModifier | StrictFPModifier | PureModifier ).*
   */
  private def parseClassModifiers: Try[List[ClassModifier]] = ???

  /* AnnotationModifier
   *  : FullAnnotation             ; '@' # QualifiedName '(' ( Identifier '=' AnnotationArgument ).*','*  ')'
   *  | SingleElementAnnotation    ; '@' # QualifiedName '(' AnnotationArgument ')'
   *  | MarkerAnnotation           ; '@' # QualifiedName
   */
  private def parseAnnotationModifier: Try[AnnotationModifier] = parseQualifiedName match {
    case Success(name) =>
      if (read('(')) {
        ???
      }
      else Success(MarkerAnnotation(name))
    case Failure(e) => Failure(e)
  }

  /* QualifiedNameOrStar
   *  : QualifiedName   ; # Identifier.*'.'*
   *  | QualifiedStar   ; # Identifier.*'.'* '.' '*'
   */
  private def parseQualifiedNameOrStar: Try[QualifiedNameOrStar] = reader.next match {
    case IdentifierToken(id, _) =>
      parseQualifiedNameOrStar(List(id))
    case token =>
      Failure(parseError("identifier", token))
  }

  /* QualifiedNameOrStar
   *  : QualifiedName   ; Identifier # ( '.' Identifier ).*
   *  | QualifiedStar   ; Identifier # ( '.' Identifier ).* '.' '*'
   */
  private def parseQualifiedNameOrStar (names: List[String]): Try[QualifiedNameOrStar] = {
    if (read('.')) reader.next match {
      case IdentifierToken(id, _) => parseQualifiedNameOrStar(names :+ id)
      case SymbolToken('*', _)    => Success(QualifiedStar(names))
      case token                  => Failure(parseError("identifier or '*'", token))
    }
    else Success(QualifiedName(names))
  }

  /* QualifiedName
   *  : # Identifier.*'.'*
   */
  private def parseQualifiedName: Try[QualifiedName] = reader.next match {
    case IdentifierToken(id, _) =>
      parseQualifiedName(List(id))
    case token =>
      Failure(parseError("identifier", token))
  }

  /* QualifiedName
   *  : Identifier # ( '.' Identifier ).*
   */
  private def parseQualifiedName (names: List[String]): Try[QualifiedName] = {
    if (read('.')) reader.next match {
      case IdentifierToken(id, _) => parseQualifiedName(names :+ id)
      case token                  => Failure(parseError("identifier", token))
    }
    else Success(QualifiedName(names))
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

case class PackageImportDeclaration (name: QualifiedStar) extends ClassImportDeclaration

sealed trait StaticImportDeclaration extends ImportDeclaration

case class StaticMemberImportDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class AllStaticMembersImportDeclaration (name: QualifiedStar) extends StaticImportDeclaration

case class DSLImportDeclaration (name: QualifiedName, precedence: Option[DSLPrecedence]) extends ImportDeclaration

sealed trait DSLPrecedence

case class AscendingDSLPrecedence (names: List[QualifiedName]) extends DSLPrecedence

case class DescendingDSLPrecedence (names: List[QualifiedName]) extends DSLPrecedence

sealed trait ModuleDeclaration

sealed trait PrjModifier

sealed trait ClassModifier extends PrjModifier
sealed trait InterfaceModifier extends PrjModifier
sealed trait DSLModifier extends PrjModifier
sealed trait ContextModifier extends PrjModifier
sealed trait FieldModifier extends PrjModifier
sealed trait MethodModifier extends PrjModifier
sealed trait OperatorModifier extends PrjModifier

case object PublicModifier extends ClassModifier with InterfaceModifier with DSLModifier with ContextModifier with FieldModifier with MethodModifier with OperatorModifier
case object PrivateModifier extends ClassModifier with InterfaceModifier with ContextModifier with FieldModifier with MethodModifier with OperatorModifier
case object ProtectedModifier extends ClassModifier with InterfaceModifier with ContextModifier with FieldModifier with MethodModifier with OperatorModifier
case object StaticModifier extends ClassModifier with InterfaceModifier with FieldModifier with MethodModifier
case object FinalModifier extends ClassModifier with ContextModifier with FieldModifier with MethodModifier with OperatorModifier
case object SynchronizedModifier extends MethodModifier with OperatorModifier
case object VolatileModifier extends FieldModifier
case object TransientModifier extends FieldModifier
case object NativeModifier extends MethodModifier with OperatorModifier
case object AbstractModifier extends ClassModifier with InterfaceModifier with ContextModifier with MethodModifier with OperatorModifier
case object StrictFPModifier extends ClassModifier with InterfaceModifier with ContextModifier with MethodModifier with OperatorModifier

case object AutoCloseModifier extends DSLModifier
case object PropagateModifier extends DSLModifier
case object PureModifier extends ClassModifier with MethodModifier with OperatorModifier

sealed trait AnnotationModifier extends ClassModifier with InterfaceModifier with DSLModifier with ContextModifier with FieldModifier with MethodModifier with OperatorModifier

case class FullAnnotation (name: QualifiedName, args: Map[String, AnnotationArgument]) extends AnnotationModifier
case class SingleElementAnnotation (name: QualifiedName, arg: AnnotationArgument) extends AnnotationModifier
case class MarkerAnnotation (name: QualifiedName) extends AnnotationModifier

case class AnnotationArgument (expression: String)


sealed trait QualifiedNameOrStar

case class QualifiedName (names: List[String]) extends QualifiedNameOrStar

case class QualifiedStar (names: List[String]) extends QualifiedNameOrStar



