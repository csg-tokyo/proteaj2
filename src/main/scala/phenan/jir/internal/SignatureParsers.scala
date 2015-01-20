package phenan.jir.internal

import phenan.jir.exception.InvalidClassFileException

import scala.util.{Try, Success => TrySucc, Failure => TryFail}
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

object SignatureParsers extends PackratParsers {
  override type Elem = Char

  def parseClassSignature (sig: String): Try[ClassSignature] = parse(sig, "class signature", classSignature)

  def parseFieldSignature (sig: String): Try[TypeSignature] = parse(sig, "field signature", fieldType)

  def parseMethodSignature (sig: String): Try[MethodSignature] = parse(sig, "method signature", methodSignature)

  private def parse[T] (sig: String, kind: String, parser: Parser[T]): Try[T] = parser(new PackratReader[Char](new CharSequenceReader(sig))) match {
    case Success(ret, _)   => TrySucc(ret)
    case NoSuccess(msg, _) => TryFail(InvalidClassFileException("fail to parse " + kind + " : " + sig + "\n" + msg))
  }

  private lazy val classSignature = formalTypeParamList ~ classType ~ classType.* ^^ {
    case typeParams ~ superClass ~ interfaces => ClassSignature(typeParams, superClass, interfaces)
  }

  private lazy val fieldType: PackratParser[TypeSignature] = classType | arrayType | typeVariable

  private lazy val methodSignature = formalTypeParamList ~ ( '(' ~> typeSignature.* <~ ')' ) ~ returnType ~ throwsType.* ^^ {
    case typeParams ~ paramTypes ~ retType ~ throwsTypes => MethodSignature(typeParams, paramTypes, retType, throwsTypes)
  }

  private lazy val typeSignature: PackratParser[TypeSignature] = fieldType | baseType

  private lazy val returnType = typeSignature | 'V' ^^^ VoidTypeSignature

  private lazy val baseType: PackratParser[PrimitiveTypeSignature] =
    'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
      'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature  | 'J' ^^^ LongTypeSignature |
      'S' ^^^ ShortTypeSignature  | 'Z' ^^^ BoolTypeSignature

  private lazy val classType = 'L' ~> classTypeSigRef <~ ';'

  private def classTypeSigRef: PackratParser[ClassTypeSignature] = new PackratParser[ClassTypeSignature] {
    override def apply(in: Input): ParseResult[ClassTypeSignature] = classTypeSig(in)
  }

  private lazy val classTypeSig: PackratParser[ClassTypeSignature] = nestedClassType | topLevelClassType

  private lazy val topLevelClassType = topLevelClass ~ typeArgList ^^ {
    case clazz ~ args => SimpleClassTypeSignature(clazz, args)
  }

  private lazy val nestedClassType = (classTypeSigRef <~ '.') ~ identifier ~ typeArgList ^^ {
    case outer ~ name ~ args => MemberClassTypeSignature(outer, name, args)
  }

  private lazy val arrayType = '[' ~> typeSignature ^^ ArrayTypeSignature

  private lazy val typeVariable = 'T' ~> identifier <~ ';' ^^ TypeVariableSignature

  private lazy val topLevelClass = repsep(identifier, '/') ^^ { ids => ids.mkString("/") }

  private lazy val formalTypeParamList = '<' ~> formalTypeParameter.+ <~ '>' | success(Nil)

  private lazy val formalTypeParameter = identifier ~ ( ':' ~> fieldType.? ) ~ ( ':' ~> fieldType ).* ^^ {
    case name ~ classBound ~ interfaceBounds => FormalTypeParameter(name, classBound, interfaceBounds)
  }

  private lazy val typeArgList = '<' ~> typeArgument.+ <~ '>' | success(Nil)

  private lazy val typeArgument = upperBoundedWildcard | lowerBoundedWildcard | unboundedWildcard | fixedTypeArgument

  private lazy val upperBoundedWildcard = '+' ~> fieldType ^^ UpperBoundWildcardArgument

  private lazy val lowerBoundedWildcard = '-' ~> fieldType ^^ LowerBoundWildcardArgument

  private lazy val unboundedWildcard = '*' ^^^ UnboundWildcardArgument

  private lazy val fixedTypeArgument = fieldType ^^ FixedTypeArgument

  private lazy val throwsType = '^' ~> (classType | typeVariable)

  private lazy val identifier: PackratParser[String] =
    elem("id start", Character.isJavaIdentifierStart) ~ elem("id part", Character.isJavaIdentifierPart).* ^^ {
      case s ~ ps => (s :: ps).mkString
    }
}
