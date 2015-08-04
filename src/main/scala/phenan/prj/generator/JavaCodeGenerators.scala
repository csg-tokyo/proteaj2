package phenan.prj.generator

import phenan.prj._
import phenan.prj.ir._
import phenan.prj.util._

import phenan.util._

import JavaRepr._

object JavaCodeGenerators extends Generators {
  lazy val javaFile: Generator[JavaFile] = ( "package" ~> string <~ ';' ).? ~ moduleDef.* ^^ { file =>
    file.packageName -> file.modules
  }

  lazy val moduleDef: Generator[ModuleDef] = classDef :|: enumDef :|: interfaceDef :|: nil

  lazy val classDef: Generator[ClassDef] = annotation.* ~ modifier ~ ( "class" ~> string ) ~ typeParam.*?('<', ',', '>') ~ ( "extends" ~> classSig ) ~ classSig.*?("implements", ',', "") ~ ( '{' ~> block(classMember.*(newLine)) <~ '}' ) ^^ { clazz =>
    clazz.annotations -> clazz.modifiers -> clazz.name -> clazz.typeParameters -> clazz.superType -> clazz.interfaces -> clazz.members
  }

  lazy val enumDef: Generator[EnumDef] = annotation.* ~ modifier ~ ( "enum" ~> string ) ~ classSig.*?("implements", ',', "") ~ ( '{' ~> block(enumConstantDef.*(',') ~ classMember.*?(';' ~> newLine, newLine, "")) <~ '}' ) ^^ { enum =>
    enum.annotations -> enum.modifiers -> enum.name -> enum.interfaces -> ( enum.constants -> enum.members )
  }

  lazy val interfaceDef: Generator[InterfaceDef] = annotation.* ~ modifier ~ ( "interface" ~> string ) ~ typeParam.*?('<', ',', '>') ~ classSig.*?("extends", ',', "") ~ ( '{' ~> block(classMember.*(newLine)) <~ '}' ) ^^ { interface =>
    interface.annotations -> interface.modifiers -> interface.name -> interface.typeParameters -> interface.superInterfaces -> interface.members
  }

  lazy val classMember: Generator[ClassMember] = ???

  lazy val enumConstantDef: Generator[EnumConstantDef] = ???


  lazy val javaLiteral: Generator[JavaLiteral] = classLiteral :|: stringLiteral :|: charLiteral :|: intLiteral :|: longLiteral :|: booleanLiteral :|: nil

  lazy val classLiteral: Generator[ClassLiteral] = ???

  lazy val stringLiteral: Generator[Literal[String]] = '\"' ~> literalChar.* <~ '\"' ^^ { _.value.toList }

  lazy val charLiteral: Generator[Literal[Char]] = '\'' ~> literalChar <~ '\'' ^^ { _.value }

  lazy val intLiteral: Generator[Literal[Int]] = elem { _.value.toString }

  lazy val longLiteral: Generator[Literal[Long]] = elem { _.value.toString + 'L' }

  lazy val booleanLiteral: Generator[Literal[Boolean]] = elem { _.value.toString }

  lazy val literalChar: Generator[Char] = elem { LiteralUtil.escape }


  lazy val typeParam: Generator[TypeParam] = ???

  lazy val classSig: Generator[ClassSig] = ???

  lazy val modifier: Generator[JModifier] = elem { _.toString }

  lazy val annotation: Generator[JavaAnnotation] = ( '@' ~> string ) ~ annotationArgument.*?('(', ',', ')') ^^ { ann =>
    ann.name -> ann.arguments.toList
  }

  lazy val annotationArgument: Generator[(String, AnnotationElement)] = string ~ ( '=' ~> annotationElement )

  lazy val annotationElement: Generator[AnnotationElement] = elementArray :|: annotation :|: javaLiteral :|: enumConstRef :|: nil

  lazy val elementArray: Generator[ElementArray] = '{' ~> annotationElement.*(',') <~ '}' ^^ { _.elements }

  lazy val enumConstRef: Generator[EnumConstRef] = elem { e =>
    e.enumName + '.' + e.constantName
  }

  /*
  lazy val classDef: Generator[IRClass] = annotation.* ~ modifiers ~ ( "class" ~> string ) ^^ { clazz =>
    clazz.annotations -> clazz.mod -> clazz.simpleName
  }

  lazy val annotation: Generator[IRAnnotation] = ( '@' ~> string ) ~ ( '(' ~> annotationArgument.*(',') <~ ')' ) ^^ { ann =>
    ann.annotationClass.name -> ann.args.toList
  }

  lazy val annotationArgument: Generator[(String, IRAnnotationElement)] = string ~ ( '=' ~> annotationElement )

  lazy val annotationElement: Generator[IRAnnotationElement] = ( '{' ~> annotationElement.*(',') <~ '}' | annotation | javaLiteral | enumConstRef ) ^^ {
    case IRAnnotationElementArray(array) => array.l.l.l
    case ann: IRAnnotation               => ann.r.l.l
    case lit: IRJavaLiteral              => lit.r.l
    case enm: IREnumConstantRef          => enm.r
  }

  lazy val enumConstRef: Generator[IREnumConstantRef] = elem { e =>
    e.field.declaringClass.name + '.' + e.field.name
  }

  lazy val javaLiteral: Generator[IRJavaLiteral] = ( classLiteral | stringLiteral | charLiteral | intLiteral | longLiteral | booleanLiteral ) ^^ {
    case cls: IRClassLiteral    => cls.l.l.l.l.l
    case str: IRStringLiteral   => str.r.l.l.l.l
    case char: IRCharLiteral    => char.r.l.l.l
    case int: IRIntLiteral      => int.r.l.l
    case long: IRLongLiteral    => long.r.l
    case bool: IRBooleanLiteral => bool.r
  }

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

  def mul (s: String, n: Int): String = (0 until n).map(_ => s).mkString
  */
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
