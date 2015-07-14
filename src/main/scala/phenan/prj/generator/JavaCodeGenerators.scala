package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._

class JavaCodeGenerators (compiler: JCompiler) extends Generators {
  lazy val classDef: Generator[IRClass] = annotation.* ~ modifiers ~ ( "class" ~> string ) ^^ { clazz =>
    clazz.annotations -> clazz.mod -> clazz.simpleName
  }

  lazy val annotation: Generator[IRAnnotation] = ( '@' ~> string ) ~ ( '(' ~> annotationArgument.*(',') <~ ')' ) ^^ { ann =>
    ann.annotationClass.name -> ann.args.toList
  }

  lazy val annotationArgument: Generator[(String, IRAnnotationElement)] = string ~ ( '=' ~> annotationElement )

  lazy val annotationElement: Generator[IRAnnotationElement] = ( annotation | annotationElementArray | annotationElementString | annotationElementClass | annotationElementEnumConst ) ^^ {
    case ann: IRAnnotation                    => ann.l.l.l.l
    case arr: IRAnnotationElementArray        => arr.r.l.l.l
    case str: IRAnnotationElementString       => str.r.l.l
    case cls: IRAnnotationElementClass        => cls.r.l
    case enm: IRAnnotationElementEnumConstant => enm.r
  }

  lazy val annotationElementArray: Generator[IRAnnotationElementArray] = '{' ~> annotationElement.*(',') <~ '}' ^^ { _.array }

  lazy val annotationElementString: Generator[IRAnnotationElementString] = stringLiteral ^^ { _.str }

  lazy val annotationElementClass: Generator[IRAnnotationElementClass] = ???

  lazy val annotationElementEnumConst: Generator[IRAnnotationElementEnumConstant] = ???

  lazy val modifiers: Generator[JModifier] = ???

  lazy val stringLiteral: Generator[String] = ???

  lazy val typeArgument: Generator[JTypeArgument] = (typeSignature | wildcardArgument) ^? {
    case sig: JTypeSignature        => sig.l
    case wld: WildcardArgument      => wld.r
  }

  lazy val typeSignature: Generator[JTypeSignature] = (primitiveTypeSignature | classTypeSignature | arrayTypeSignature | typeVariableSignature) ^? {
    case pts: JPrimitiveTypeSignature => pts.l.l.l
    case cts: JClassTypeSignature     => cts.r.l.l
    case ats: JArrayTypeSignature     => ats.r.l
    case tvs: JTypeVariableSignature  => tvs.r
  }

  lazy val classTypeSignature: Generator[JClassTypeSignature] = (simpleClassTypeSignature | memberClassTypeSignature) ^^ {
    case sig: SimpleClassTypeSignature => sig.l
    case sig: MemberClassTypeSignature => sig.r
  }

  lazy val simpleClassTypeSignature: Generator[SimpleClassTypeSignature] = string ~ typeArgument.*?('<', ',', '>') ^^ { sig =>
    sig.clazz.replace('/', '.') -> sig.args
  }

  lazy val memberClassTypeSignature: Generator[MemberClassTypeSignature] = classTypeSignature ~ ( '.' ~> string ) ~ typeArgument.*?('<', ',', '>') ^^ { sig =>
    sig.outer -> sig.clazz -> sig.args
  }

  lazy val primitiveTypeSignature: Generator[JPrimitiveTypeSignature] = elem {
    case ByteTypeSignature   => "byte"
    case CharTypeSignature   => "char"
    case DoubleTypeSignature => "double"
    case FloatTypeSignature  => "float"
    case IntTypeSignature    => "int"
    case LongTypeSignature   => "long"
    case ShortTypeSignature  => "short"
    case BoolTypeSignature   => "boolean"
    case VoidTypeSignature   => "void"
  }

  lazy val arrayTypeSignature: Generator[JArrayTypeSignature] = typeSignature <~ '[' <~ ']' ^^ { _.component }

  lazy val typeVariableSignature: Generator[JTypeVariableSignature] = elem(_.name)

  lazy val wildcardArgument: Generator[WildcardArgument] = '?' ~> ( "extends" ~> typeSignature ).? ~ ( "super" ~> typeSignature ).? ^^ { arg =>
    arg.upperBound -> arg.lowerBound
  }

  val spacingBeforeWord: List[Char] = List('?', '(', '{')

  val spacingAfterWord: List[Char] = List(',', ')', '}', '>')
}
