package phenan.prj.signature

import phenan.prj._
import phenan.prj.state.JState

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.CharSequenceReader

trait SignatureParser {
  this: Application =>

  object SignatureParsers extends PackratParsers {
    override type Elem = Char

    lazy val classSignature: Parser[JClassSignature] = formalTypeParamList ~ classType ~ classType.* ^^ {
      case typeParams ~ superClass ~ interfaces => JClassSignature(typeParams, superClass, interfaces)
    }

    lazy val fieldType: PackratParser[JTypeSignature] = classType | arrayType | typeVariable

    lazy val methodSignature: PackratParser[JMethodSignature] = formalTypeParamList ~ ('(' ~> parameterSignature.* <~ ')') ~ returnType ~ throwsType.* ^^ {
      case typeParams ~ parameters ~ retType ~ throwsTypes => JMethodSignature(typeParams, parameters, retType, Nil, throwsTypes, Nil, Nil, Nil)
    }

    lazy val parameterSignature: PackratParser[JParameterSignature] = ('@' ~> typeSignature).* ~ typeSignature ~ ('*'.? ^^ {
      _.nonEmpty
    }) ~ ('?' ~> identifier).? ^^ {
      case contexts ~ sig ~ va ~ df => JParameterSignature(contexts, sig, va, df)
    }

    lazy val typeSignature: PackratParser[JTypeSignature] = fieldType | baseType

    lazy val returnType: PackratParser[JTypeSignature] = typeSignature | 'V' ^^^ VoidTypeSignature

    lazy val baseType: PackratParser[JPrimitiveTypeSignature] =
      'B' ^^^ ByteTypeSignature | 'C' ^^^ CharTypeSignature | 'D' ^^^ DoubleTypeSignature |
        'F' ^^^ FloatTypeSignature | 'I' ^^^ IntTypeSignature | 'J' ^^^ LongTypeSignature |
        'S' ^^^ ShortTypeSignature | 'Z' ^^^ BoolTypeSignature

    lazy val classType: PackratParser[JClassTypeSignature] = 'L' ~> classTypeSigRef <~ ';'

    def classTypeSigRef: PackratParser[JClassTypeSignature] = new PackratParser[JClassTypeSignature] {
      override def apply(in: Input): ParseResult[JClassTypeSignature] = classTypeSig(in)
    }

    lazy val classTypeSig: PackratParser[JClassTypeSignature] = nestedClassType | topLevelClassType

    lazy val topLevelClassType: PackratParser[SimpleClassTypeSignature] = topLevelClass ~ typeArgList ^^ {
      case clazz ~ args => SimpleClassTypeSignature(clazz, args)
    }

    lazy val nestedClassType: PackratParser[MemberClassTypeSignature] = (classTypeSigRef <~ '.') ~ identifier ~ typeArgList ^^ {
      case outer ~ name ~ args => MemberClassTypeSignature(outer, name, args)
    }

    lazy val arrayType: PackratParser[JArrayTypeSignature] = '[' ~> typeSignature ^^ JArrayTypeSignature

    lazy val typeVariable: Parser[JTypeVariableSignature] = 'T' ~> identifier <~ ';' ^^ JTypeVariableSignature

    lazy val topLevelClass: Parser[String] = repsep(identifier, '/') ^^ { ids => ids.mkString("/") }

    lazy val formalTypeParamList: PackratParser[List[FormalMetaParameter]] = '<' ~> formalTypeParameter.+ <~ '>' | success(Nil)

    lazy val formalTypeParameter: Parser[FormalMetaParameter] = identifier ~ (':' ~> fieldType.?) ~ (':' ~> fieldType).* ^^ {
      case name ~ classBound ~ interfaceBounds =>
        FormalMetaParameter(name, JTypeSignature.typeTypeSig, classBound.map(_ :: interfaceBounds).getOrElse(interfaceBounds))
    }

    lazy val typeArgList: Parser[List[JTypeArgument]] = '<' ~> typeArgument.+ <~ '>' | success(Nil)

    lazy val typeArgument: PackratParser[JTypeArgument] = upperBoundedWildcard | lowerBoundedWildcard | unboundedWildcard | pureVariable | fieldType

    lazy val upperBoundedWildcard: Parser[WildcardArgument] = '+' ~> fieldType ^^ { ub => WildcardArgument(Some(ub), None) }

    lazy val lowerBoundedWildcard: Parser[WildcardArgument] = '-' ~> fieldType ^^ { lb => WildcardArgument(None, Some(lb)) }

    lazy val unboundedWildcard: Parser[WildcardArgument] = '*' ^^^ WildcardArgument(None, None)

    lazy val pureVariable: Parser[MetaVariableSignature] = 'P' ~> identifier <~ ';' ^^ MetaVariableSignature

    lazy val throwsType: PackratParser[JTypeSignature] = '^' ~> (classType | typeVariable)

    lazy val identifier: PackratParser[String] =
      elem("id start", Character.isJavaIdentifierStart) ~ elem("id part", Character.isJavaIdentifierPart).* ^^ {
        case s ~ ps => (s :: ps).mkString
      }

    def parse[T](sig: String, kind: String, parser: Parser[T]): Option[T] = parser(new PackratReader[Char](new CharSequenceReader(sig))) match {
      case Success(ret, _) => Some(ret)
      case NoSuccess(msg, _) => errorAndReturn("fail to parse " + kind + " : " + sig + "\n" + msg, None)
    }
  }

  import SignatureParsers._

  def parseClassSignature(sig: String): Option[JClassSignature] = parse(sig, "class signature", classSignature)
  def parseFieldSignature(sig: String): Option[JTypeSignature] = parse(sig, "field signature", fieldType)
  def parseMethodSignature(sig: String): Option[JMethodSignature] = parse(sig, "method signature", methodSignature)
  def parseTypeSignature(sig: String): Option[JTypeSignature] = parse(sig, "type signature", typeSignature)
  def parseClassTypeSignature(sig: String): Option[JClassTypeSignature] = parse(sig, "class type signature", classType)
  def parseParameterSignature(sig: String): Option[JParameterSignature] = parse(sig, "parameter signature", parameterSignature)
}
