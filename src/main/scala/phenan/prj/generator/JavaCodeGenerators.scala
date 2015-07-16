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

  lazy val annotationElement: Generator[IRAnnotationElement] = ( '{' ~> annotationElement.*(',') <~ '}' | annotation | javaLiteral | annotationElementEnumConst ) ^^ {
    case IRAnnotationElementArray(array)      => array.l.l.l
    case ann: IRAnnotation                    => ann.r.l.l
    case lit: IRJavaLiteral                   => lit.r.l
    case enm: IRAnnotationElementEnumConstant => enm.r
  }

  lazy val annotationElementEnumConst: Generator[IRAnnotationElementEnumConstant] = ???

  lazy val javaLiteral: Generator[IRJavaLiteral] = ???

  lazy val stringLiteral: Generator[IRStringLiteral] = ???

  lazy val classLiteral: Generator[IRClassLiteral] = ( objectClassLiteral | primitiveClassLiteral ) ^^ {
    case obj: IRObjectClassLiteral    => obj.l
    case prm: IRPrimitiveClassLiteral => prm.r
  }

  lazy val objectClassLiteral: Generator[IRObjectClassLiteral] = elem { obj =>
    obj.clazz.name + mul("[]", obj.dim) + ".class"
  }

  lazy val primitiveClassLiteral: Generator[IRPrimitiveClassLiteral] = elem { prm =>
    prm.primitiveClass.name + mul("[]", prm.dim) + ".class"
  }

  lazy val modifiers: Generator[JModifier] = ???

  def mul (s: String, n: Int): String = (0 until n).map(_ => s).mkString

  /*
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
*/
  val spacingBeforeWord: List[Char] = List('?', '(', '{')

  val spacingAfterWord: List[Char] = List(',', ')', '}', '>')
}
