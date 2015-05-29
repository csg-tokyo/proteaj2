package phenan.prj.internal

import phenan.prj._
import phenan.prj.state.JState

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

object SignatureParsers extends PackratParsers {
  override type Elem = Char

  def parseClassSignature (sig: String)(implicit state: JState): Option[JClassSignature] = parse(sig, "class signature", classSignature)

  def parseFieldSignature (sig: String)(implicit state: JState): Option[JTypeSignature] = parse(sig, "field signature", fieldType)

  def parseMethodSignature (sig: String)(implicit state: JState): Option[JMethodSignature] = parse(sig, "method signature", methodSignature)

  def parseTypeSignature (sig: String)(implicit state: JState): Option[JTypeSignature] = parse(sig, "type signature", typeSignature)

  def parseClassTypeSignature (sig: String)(implicit state: JState): Option[JClassTypeSignature] = parse(sig, "class type signature", classType)

  def parseParameterSignature (sig: String)(implicit state: JState): Option[JParameterSignature] = parse(sig, "parameter signature", parameterSignature)

  private def parse[T] (sig: String, kind: String, parser: Parser[T])(implicit state: JState): Option[T] = parser(new PackratReader[Char](new CharSequenceReader(sig))) match {
    case Success(ret, _)   => Some(ret)
    case NoSuccess(msg, _) =>
      state.error("fail to parse " + kind + " : " + sig + "\n" + msg)
      None
  }

  private lazy val classSignature = formalTypeParamList ~ classType ~ classType.* ^^ {
    case typeParams ~ superClass ~ interfaces => JClassSignature(typeParams, superClass, interfaces)
  }

  private lazy val fieldType: PackratParser[JTypeSignature] = classType | arrayType | typeVariable

  private lazy val methodSignature = formalTypeParamList ~ ( '(' ~> parameterSignature.* <~ ')' ) ~ returnType ~ throwsType.* ^^ {
    case typeParams ~ parameters ~ retType ~ throwsTypes => JMethodSignature(typeParams, parameters, retType, throwsTypes, Nil, Nil, Nil)
  }

  private lazy val parameterSignature = ( '@' ~> typeSignature ).* ~ typeSignature ~ ( '#' ~> identifier ).? ~ ( '*'.? ^^ { _.nonEmpty } ) ~ ( '?' ~> identifier ).? ^^ {
    case contexts ~ sig ~ pri ~ va ~ df => JParameterSignature(contexts, sig, pri, va, df)
  }

  private lazy val typeSignature: PackratParser[JTypeSignature] = fieldType | baseType

  private lazy val returnType = typeSignature | 'V' ^^^ VoidTypeSignature

  private lazy val baseType: PackratParser[JPrimitiveTypeSignature] =
    'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
      'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature  | 'J' ^^^ LongTypeSignature |
      'S' ^^^ ShortTypeSignature  | 'Z' ^^^ BoolTypeSignature

  private lazy val classType = 'L' ~> classTypeSigRef <~ ';'

  private def classTypeSigRef: PackratParser[JClassTypeSignature] = new PackratParser[JClassTypeSignature] {
    override def apply(in: Input): ParseResult[JClassTypeSignature] = classTypeSig(in)
  }

  private lazy val classTypeSig: PackratParser[JClassTypeSignature] = nestedClassType | topLevelClassType

  private lazy val topLevelClassType = topLevelClass ~ typeArgList ^^ {
    case clazz ~ args => SimpleClassTypeSignature(clazz, args)
  }

  private lazy val nestedClassType = (classTypeSigRef <~ '.') ~ identifier ~ typeArgList ^^ {
    case outer ~ name ~ args => MemberClassTypeSignature(outer, name, args)
  }

  private lazy val arrayType = '[' ~> typeSignature ^^ JArrayTypeSignature

  private lazy val typeVariable = 'T' ~> identifier <~ ';' ^^ JTypeVariableSignature

  private lazy val topLevelClass = repsep(identifier, '/') ^^ { ids => ids.mkString("/") }

  private lazy val formalTypeParamList = '<' ~> formalTypeParameter.+ <~ '>' | success(Nil)

  private lazy val formalTypeParameter = identifier ~ ( ':' ~> fieldType.? ) ~ ( ':' ~> fieldType ).* ^^ {
    case name ~ classBound ~ interfaceBounds =>
      FormalMetaParameter(name, JTypeSignature.typeTypeSig, None, classBound.map(_ :: interfaceBounds).getOrElse(interfaceBounds))
  }

  private lazy val typeArgList = '<' ~> typeArgument.+ <~ '>' | success(Nil)

  private lazy val typeArgument = upperBoundedWildcard | lowerBoundedWildcard | unboundedWildcard | pureVariable | fieldType

  private lazy val upperBoundedWildcard = '+' ~> fieldType ^^ UpperBoundWildcardArgument

  private lazy val lowerBoundedWildcard = '-' ~> fieldType ^^ LowerBoundWildcardArgument

  private lazy val unboundedWildcard = '*' ^^^ UnboundWildcardArgument

  private lazy val pureVariable = 'P' ~> identifier <~ ';' ^^ PureVariableSignature

  private lazy val throwsType = '^' ~> (classType | typeVariable)

  private lazy val identifier: PackratParser[String] =
    elem("id start", Character.isJavaIdentifierStart) ~ elem("id part", Character.isJavaIdentifierPart).* ^^ {
      case s ~ ps => (s :: ps).mkString
    }
}
