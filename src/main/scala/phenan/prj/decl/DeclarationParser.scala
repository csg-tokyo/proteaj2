package phenan.prj.decl

import java.io._

import phenan.prj.exception.ParseException
import phenan.prj.state.JState

import scala.util._

@Deprecated
object DeclarationParser {
  def apply (reader: Reader, fileName: String)(implicit state: JState): Try[DeclarationParser] = {
    SourceReader(reader, fileName).map(new DeclarationParser(_, fileName))
  }
}

@Deprecated
class DeclarationParser private (private val reader: SourceReader, private val fileName: String)(implicit state: JState) {
  lazy val parseAll: CompilationUnit = {
    val cu = parseCompilationUnit
    reader.close()
    cu
  }

  /* BNF-like notation
   *
   * # indicates current position
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
      Success(ImportStaticStarDeclaration(name))
    }
    else if (read(';')) Success(ImportStaticMemberDeclaration(name))
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
    else parseModifiers.flatMap(parseModuleDeclaration) match {
      case Success(m) => parseModuleDeclarations(modules :+ m)
      case Failure(e) =>
        state.error("parse error : invalid module declaration", e)
        reader.skipBlock(pos)
        parseModuleDeclarations(modules)
    }
  }

  /* ModuleDeclaration
   *  : ClassDeclaration         ; # Modifiers "class" Identifier List[TypeParameter] ExtendsClass ImplementsInterfaces ClassBody
   *  | EnumDeclaration          ; # Modifiers "enum" Identifier ImplementsInterfaces EnumBody
   *  | InterfaceDeclaration     ; # Modifiers "interface" Identifier List[TypeParameter] ExtendsInterfaces.? InterfaceBody
   *  | AnnotationDeclaration    ; # Modifiers '@' "interface" Identifier AnnotationBody
   *  | DSLDeclaration           ; # Modifiers "dsl" Identifier DependsDSLs DSLBody
   */
  private def parseModuleDeclaration (modifiers: List[Modifier]): Try[ModuleDeclaration] = {
    if (read("class")) parseClassDeclaration(modifiers)
    else if (read("enum")) parseEnumDeclaration(modifiers)
    else if (read("interface")) parseInterfaceDeclaration(modifiers)
    else if (read("dsl")) parseDSLDeclaration(modifiers)
    else if (reader.look(0).is('@') && reader.look(1).is("interface")) {
      reader.next(1)
      parseAnnotationDeclaration(modifiers)
    }
    else Failure(parseError("class, enum, interface, @interface, or dsl"))
  }


  /* ClassDeclaration
   *  : Modifiers "class" # Identifier List[TypeParameter] ExtendsClass ImplementsInterfaces ClassBody
   */
  private def parseClassDeclaration (modifiers: List[Modifier]): Try[ClassDeclaration] = for {
    name           <- parseIdentifier
    typeParameters <- parseTypeParameters
    extendsClass   <- parseExtendsClass
    interfaces     <- parseImplementsInterfaces
    members        <- parseClassBody(name)
  } yield ClassDeclaration(modifiers, name, typeParameters, extendsClass, interfaces, members)

  /* EnumDeclaration
   *  : Modifiers "enum" # Identifier ImplementsInterfaces EnumBody
   */
  private def parseEnumDeclaration (modifiers: List[Modifier]): Try[EnumDeclaration] = ???

  /* InterfaceDeclaration
   *  : Modifiers "interface" # Identifier List[TypeParameter] ExtendsInterfaces.? InterfaceBody
   */
  private def parseInterfaceDeclaration (modifiers: List[Modifier]): Try[InterfaceDeclaration] = ???

  /* AnnotationDeclaration
   *  : Modifiers '@' "interface" # Identifier AnnotationBody
   */
  private def parseAnnotationDeclaration (modifiers: List[Modifier]): Try[AnnotationDeclaration] = ???

  /* DSLDeclaration
   *  : Modifiers "dsl" Identifier DependsDSLs DSLBody
   */
  private def parseDSLDeclaration (modifiers: List[Modifier]): Try[DSLDeclaration] = ???

  /* List[TypeParameter]
   *  : # ( '<' TypeParameter ( ',' TypeParameter ).* '>' ).?
   */
  private def parseTypeParameters: Try[List[TypeParameter]] = {
    if (read('<')) parseTypeParameter.flatMap { param => parseTypeParameters(List(param)) }
    else Success(Nil)
  }

  /* List[TypeParameter]
   *  : ( '<' TypeParameter # ( ',' TypeParameter ).* '>' ).?
   */
  private def parseTypeParameters (params: List[TypeParameter]): Try[List[TypeParameter]] = {
    if (read(',')) parseTypeParameter match {
      case Success(param) => parseTypeParameters(params :+ param)
      case Failure(e)     => Failure(e)
    }
    else if (read('>')) Success(params)
    else Failure(parseError("',' or '>'"))
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
  private def parseClassBody(className: String): Try[List[ClassMember]] = parseToken('{').flatMap(_ => parseClassBody(className, Nil))

  /* ClassBody
   *  : '{' # ClassMember.* '}'
   */
  private def parseClassBody(className: String, members: List[ClassMember]): Try[List[ClassMember]] = {
    if (read('}')) Success(members)
    else parseClassMember(className) match {
      case Success(member) => parseClassBody(className, members :+ member)
      case Failure(e) => Failure(e)
    }
  }

  /* ClassMember
   *  : InstanceInitializer      ; # '{' BlockSnippet '}'
   *  | StaticInitializer        ; # "static" '{' BlockSnippet '}'
   *  | ConstructorDeclaration   ; # Modifiers List[TypeParameter] TypeName List[FormalParameter] ThrowsExceptions '{' BlockSnippet '}'
   *  | MethodDeclaration        ; # Modifiers List[TypeParameter] TypeName Identifier List[FormalParameter] ThrowsExceptions MethodBody
   *  | FieldDeclaration         ; # Modifiers TypeName List[VariableDeclarator] ';'
   *  | ClassDeclaration         ; # Modifiers "class" Identifier List[TypeParameter] ExtendsClass ImplementsInterfaces ClassBody
   *  | EnumDeclaration          ; # Modifiers "enum" Identifier ImplementsInterfaces EnumBody
   *  | InterfaceDeclaration     ; # Modifiers "interface" Identifier List[TypeParameter] ExtendsInterfaces.? InterfaceBody
   *  | AnnotationDeclaration    ; # Modifiers '@' "interface" Identifier AnnotationBody
   *  | DSLDeclaration           ; # Modifiers "dsl" Identifier DependsDSLs DSLBody
   */
  private def parseClassMember(className: String): Try[ClassMember] = {
    if (read('{')) parseInstanceInitializer
    else if (reader.look(0).is("static") && reader.look(1).is('{')) {
      reader.next(1)
      parseStaticInitializer
    }
    else parseModifiers.flatMap { modifiers =>
      if (read("class")) parseClassDeclaration(modifiers)
      else if (read("enum")) parseEnumDeclaration(modifiers)
      else if (read("interface")) parseInterfaceDeclaration(modifiers)
      else if (read("dsl")) parseDSLDeclaration(modifiers)
      else if (reader.look(0).is('@') && reader.look(1).is("interface")) {
        reader.next(1)
        parseAnnotationDeclaration(modifiers)
      }
      else parseTypeParameters.flatMap {
        case Nil => parseTypeName.flatMap { typeName =>
          if (sameClassName(typeName, className) && reader.head.is('(')) parseConstructorDeclaration(modifiers, Nil)
          else if (reader.look(1).is('(')) parseMethodDeclaration(modifiers, Nil, typeName)
          else parseFieldDeclaration(modifiers, typeName)
        }
        case typeParameters => parseTypeName.flatMap { typeName =>
          if (sameClassName(typeName, className) && reader.head.is('(')) parseConstructorDeclaration(modifiers, typeParameters)
          else parseMethodDeclaration(modifiers, typeParameters, typeName)
        }
      }
    }
  }

  private def sameClassName (typeName: TypeName, className: String): Boolean = typeName match {
    case ClassTypeName(QualifiedName(List(simpleName)), Nil) if simpleName == className => true
    case _ => false
  }

  /* InstanceInitializer
   *  : '{' # BlockSnippet '}'
   */
  private def parseInstanceInitializer: Try[InstanceInitializer] = for {
    block <- parseBlockSnippet
    _     <- parseToken('}')
  } yield InstanceInitializer(block)

  /* StaticInitializer
   *  : "static" '{' # BlockSnippet '}'
   */
  private def parseStaticInitializer: Try[StaticInitializer] = for {
    block <- parseBlockSnippet
    _     <- parseToken('}')
  } yield StaticInitializer(block)

  /* ConstructorDeclaration
   *  : Modifiers List[TypeParameter] TypeName # List[FormalParameter] ThrowsExceptions '{' BlockSnippet '}'
   */
  private def parseConstructorDeclaration (modifiers: List[Modifier], typeParameters: List[TypeParameter]): Try[ConstructorDeclaration] = for {
    formalParameters <- parseFormalParameters
    throwsExceptions <- parseThrowsExceptions
    _                <- parseToken('{')
    blockSnippet     <- parseBlockSnippet
    _                <- parseToken('}')
  } yield ConstructorDeclaration(modifiers, typeParameters, formalParameters, throwsExceptions, blockSnippet)

  /* MethodDeclaration
   *  : Modifiers List[TypeParameter] TypeName # Identifier List[FormalParameter] ThrowsExceptions MethodBody
   */
  private def parseMethodDeclaration (modifiers: List[Modifier], typeParameters: List[TypeParameter], returnType: TypeName): Try[MethodDeclaration] = for {
    name             <- parseIdentifier
    formalParameters <- parseFormalParameters
    throwsExceptions <- parseThrowsExceptions
    body             <- parseMethodBody
  } yield MethodDeclaration(modifiers, typeParameters, returnType, name, formalParameters, throwsExceptions, body)

  /* FieldDeclaration
   *  : Modifiers TypeName # List[VariableDeclarator] ';'
   */
  private def parseFieldDeclaration (modifiers: List[Modifier], fieldType: TypeName): Try[FieldDeclaration] = for {
    declarators <- parseVariableDeclarators
    _           <- parseToken(';')
  } yield FieldDeclaration(modifiers, fieldType, declarators)

  /* List[FormalParameter]
   *  : # '(' ( FormalParameter ( ',' FormalParameter ) ).? ')'
   */
  private def parseFormalParameters: Try[List[FormalParameter]] = {
    if (read('(')) {
      if (read(')')) Success(Nil)
      else parseFormalParameter.flatMap(param => parseFormalParameters(List(param)))
    }
    else Failure(parseError("formal parameters"))
  }

  /* List[FormalParameter]
   *  : '(' ( FormalParameter # ( ',' FormalParameter ) ).? ')'
   */
  private def parseFormalParameters(parameters: List[FormalParameter]): Try[List[FormalParameter]] = {
    if (read(',')) parseFormalParameter match {
      case Success(param) => parseFormalParameters(parameters :+ param)
      case Failure(e)     => Failure(e)
    }
    else if (read(')')) Success(parameters)
    else Failure(parseError("',' or ')'"))
  }

  /* FormalParameter
   *  : Modifiers TypeName VariableArgumentsIndicator Identifier EmptyBrackets Initializer
   */
  private def parseFormalParameter: Try[FormalParameter] = for {
    modifiers     <- parseModifiers
    parameterType <- parseTypeName
    varArgs       <- parseVariableArgumentsIndicator
    name          <- parseIdentifier
    dim           <- parseEmptyBrackets
    initializer   <- parseInitializer
  } yield FormalParameter(modifiers, parameterType, varArgs, name, dim, initializer)

  /* VariableArgumentsIndicator
   *  : ( '.' '.' '.' ).?
   */
  private def parseVariableArgumentsIndicator: Try[Boolean] = {
    if (reader.look(0).is('.') && reader.look(1).is('.') && reader.look(2).is('.')) {
      reader.next(2)
      Success(true)
    }
    else Success(false)
  }

  /* ThrowsExceptions
   *  : # ( "throws" ClassTypeName ( ',' ClassTypeName ).* ).?
   */
  private def parseThrowsExceptions: Try[List[ClassTypeName]] = {
    if (read("throws")) parseClassTypeName.flatMap(e => parseThrowsExceptions(List(e)))
    else Success(Nil)
  }

  /* ThrowsExceptions
   *  : ( "throws" ClassTypeName # ( ',' ClassTypeName ).* ).?
   */
  private def parseThrowsExceptions (exceptions: List[ClassTypeName]): Try[List[ClassTypeName]] = {
    if (read(',')) parseClassTypeName match {
      case Success(e) => parseThrowsExceptions(exceptions :+ e)
      case Failure(e) => Failure(e)
    }
    else Success(exceptions)
  }

  /* MethodBody
   *  : ';'
   *  | '{' BlockSnippet '}'
   */
  private def parseMethodBody: Try[Option[BlockSnippet]] = {
    if (read(';')) Success(None)
    else if (read('{')) for {
      block <- parseBlockSnippet
      _     <- parseToken('}')
    } yield Some(block)
    else Failure(parseError("';' or '{'"))
  }

  /* List[VariableDeclarator]
   *  : # VariableDeclarator ( ',' VariableDeclarator ).*
   */
  private def parseVariableDeclarators: Try[List[VariableDeclarator]] = parseVariableDeclarator.flatMap(v => parseVariableDeclarators(List(v)))

  /* List[VariableDeclarator]
   *  : VariableDeclarator # ( ',' VariableDeclarator ).*
   */
  private def parseVariableDeclarators(declarators: List[VariableDeclarator]): Try[List[VariableDeclarator]] = {
    if (read(',')) parseVariableDeclarator match {
      case Success(v) => parseVariableDeclarators(declarators :+ v)
      case Failure(e) => Failure(e)
    }
    else Success(declarators)
  }

  /* VariableDeclarator
   *  : # Identifier EmptyBrackets Initializer
   */
  private def parseVariableDeclarator: Try[VariableDeclarator] = for {
    name <- parseIdentifier
    dim  <- parseEmptyBrackets
    ini  <- parseInitializer
  } yield VariableDeclarator(name, dim, ini)

  /* EmptyBrackets
   *  : # ( '[' ']' ).*
   */
  private def parseEmptyBrackets: Try[Int] = parseEmptyBrackets(0)

  private def parseEmptyBrackets (n: Int) : Try[Int] = {
    if (read('[')) {
      if (read(']')) parseEmptyBrackets(n + 1)
      else Failure(parseError("']'"))
    }
    else Success(n)
  }

  /* Initializer
   *  : ( '=' ExpressionSnippet ).?
   */
  private def parseInitializer: Try[Option[ExpressionSnippet]] = {
    if (read('=')) parseExpressionSnippet.map(Some(_))
    else Success(None)
  }

  /* Modifiers
   *  : ( Annotation        | PublicModifier    | PrivateModifier      | ProtectedModifier
   *    | StaticModifier    | FinalModifier     | SynchronizedModifier | VolatileModifier
   *    | TransientModifier | NativeModifier    | AbstractModifier     | StrictFPModifier
   *    | AutoCloseModifier | PropagateModifier | PureModifier ).*
   */
  private def parseModifiers: Try[List[Modifier]] = parseModifiers(Nil)

  private def parseModifiers (list: List[Modifier]): Try[List[Modifier]] = reader.head match {
    case SymbolToken('@', _) if ! reader.look(1).is("interface") =>
      reader.next  // '@'
      parseAnnotation match {
        case Success(ann) => parseModifiers(list :+ ann)
        case Failure(e) => Failure(e)
      }
    case IdentifierToken(id, _) if modifiers.contains(id) =>
      reader.next  // id
      parseModifiers(list :+ modifiers(id))
    case _ => Success(list)
  }

  private val modifiers: Map[String, Modifier] = Map(
    "public" -> PublicModifier, "private" -> PrivateModifier, "protected" -> ProtectedModifier,
    "static" -> StaticModifier, "final" -> FinalModifier, "synchronized" -> SynchronizedModifier,
    "volatile" -> VolatileModifier, "transient" -> TransientModifier, "native" -> NativeModifier,
    "abstract" -> AbstractModifier, "strictfp" -> StrictFPModifier, "pure" -> PureModifier
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
        reader.next(1)
        parseQualifiedName(names :+ id)
      case _ => QualifiedName(names)
    }
    else QualifiedName(names)
  }

  /* ExpressionSnippet
   *  : # ... ( ')' | '}' | ',' | ';' )
   *
   * Note.
   * (1) this method do NOT read the last ( ')' | '}' | ',' | ';' )
   * (2) the last ( ')' | '}' | ',' | ';' ) is not included in ExpressionSnippet.
   * (3) all parentheses and curly braces opened in ... must be closed at the inside of ...
   */
  private def parseExpressionSnippet: Try[ExpressionSnippet] = reader.nextExpression.map(new ExpressionSnippet(_))

  /* BlockSnippet
   *  : # ... '}'
   *
   * Note.
   * (1) this method do NOT read the last '}'
   * (2) the last '}' is not included in BlockSnippet.
   * (3) all curly braces opened in ... must be closed at the inside of ...
   */
  private def parseBlockSnippet: Try[BlockSnippet] = reader.nextBlock.map(new BlockSnippet(_))

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

  private def parseError (expected: String): ParseException = parseError(expected, reader.head)

  private def parseError (expected: String, found: SourceToken): ParseException = {
    ParseException(expected, found.toString, fileName, found.line)
  }
}
