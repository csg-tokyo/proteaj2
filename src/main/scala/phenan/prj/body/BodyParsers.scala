package phenan.prj.body

import phenan.prj._
import phenan.prj.combinator._
import phenan.prj.ir._

import scala.language.implicitConversions
import scala.util._

import scalaz.Memo._

class BodyParsers (compiler: JCompiler) extends TwoLevelParsers {
  type Elem = Char

  class StatementParsers private (returnType: JType, env: Environment) {
    lazy val block = '{' ~> blockStatements <~ '}' ^^ IRBlock
    lazy val blockStatements: HParser[List[IRStatement]] = statement_BlockStatements | local_BlockStatements | expression_BlockStatements

    private lazy val statement_BlockStatements = statement ~ blockStatements ^^ { case s ~ bs => s :: bs }
    private lazy val local_BlockStatements = localDeclaration >> { local => StatementParsers(returnType, env.defineLocals(local)).blockStatements ^^ { local :: _ } }
    private lazy val expression_BlockStatements = expressionStatement >> { es => StatementParsers(returnType, env.modifyContext(es)).blockStatements ^^ { es :: _ } }

    lazy val statement: HParser[IRStatement] = block | controlStatement
    lazy val controlStatement: HParser[IRStatement] = ???
    lazy val localDeclaration: HParser[IRLocalDeclaration] = ???
    lazy val variableDeclarator: HParser[IRVariableDeclarator] = ???
    lazy val expressionStatement = ExpressionParsers(compiler.typeLoader.voidType, env).expression <~ ';' ^^ IRExpressionStatement
  }

  class ExpressionParsers private (expected: JType, env: Environment) {
    lazy val expression: HParser[IRExpression] = ???
  }

  class TypeParsers private (resolver: NameResolver) {
    lazy val metaValue: HParser[MetaValue] = wildcard | metaVariable | refType
    lazy val typeName: HParser[JType] = primitiveTypeName | refType
    lazy val refType: HParser[JRefType] = arrayType | typeVariable | objectType
    lazy val objectType: HParser[JObjectType] = className ~ ( '<' ~> metaValue.+(',') <~ '>' ).? ^^? {
      case clazz ~ args => clazz.objectType(args.getOrElse(Nil))
    }
    lazy val packageName: HParser[List[String]] = (identifier <~ '.').*! { names =>
      ! resolver.root.isKnownPackage(names) && resolver.resolve(names).isSuccess
    }
    lazy val className: HParser[JClass] = ref(innerClassName | topLevelClassName)
    lazy val topLevelClassName: HParser[JClass] = packageName ~ identifier ^^? {
      case pack ~ name => resolver.resolve(pack :+ name).toOption
    }
    lazy val innerClassName: HParser[JClass] = className ~ ('.' ~> identifier) ^^? {
      case name ~ id => name.innerClasses.get(id).flatMap(compiler.classLoader.loadClass_PE)
    }
    lazy val typeVariable: HParser[JTypeVariable] = identifier ^^? resolver.typeVariable
    lazy val metaVariable: HParser[PureVariableRef] = identifier ^^? resolver.metaVariable
    lazy val arrayType: HParser[JArrayType] = typeName <~ '[' <~ ']' ^^ { _.array }
    lazy val primitiveTypeName: HParser[JPrimitiveType] = identifier ^? {
      case "byte"    => compiler.classLoader.byte.primitiveType
      case "char"    => compiler.classLoader.char.primitiveType
      case "double"  => compiler.classLoader.double.primitiveType
      case "float"   => compiler.classLoader.float.primitiveType
      case "int"     => compiler.classLoader.int.primitiveType
      case "long"    => compiler.classLoader.long.primitiveType
      case "short"   => compiler.classLoader.short.primitiveType
      case "boolean" => compiler.classLoader.boolean.primitiveType
      case "void"    => compiler.classLoader.void.primitiveType
    }
    lazy val wildcard: HParser[JWildcard] = '?' ~> ( "extends" ~> refType ).? ~ ( "super" ~> refType ).? ^^ {
      case ub ~ lb => JWildcard(ub, lb)
    }
  }

  object StatementParsers {
    def apply (expected: JType, env: Environment): StatementParsers = cached((expected, env))
    private val cached : ((JType, Environment)) => StatementParsers = mutableHashMapMemo { pair => new StatementParsers(pair._1, pair._2) }
  }

  object ExpressionParsers {
    def apply (expected: JType, env: Environment): ExpressionParsers = cached((expected, env))
    private val cached : ((JType, Environment)) => ExpressionParsers = mutableHashMapMemo { pair => new ExpressionParsers(pair._1, pair._2) }
  }

  object TypeParsers {
    def apply (resolver: NameResolver): TypeParsers = cached(resolver)
    private val cached : NameResolver => TypeParsers = mutableHashMapMemo(new TypeParsers(_))
  }

  lazy val delimiter: LParser[Any] = elem("white space", Character.isWhitespace).*

  lazy val qualifiedName = identifier.+('.')

  lazy val identifier = (elem("identifier start", Character.isJavaIdentifierStart) ~ elem("identifier part", Character.isJavaIdentifierPart).*).^ ^^ {
    case s ~ ps => (s :: ps).mkString
  }

  private def word (cs: String): LParser[String] = cs.foldRight(success(cs)) { (ch, r) => elem(ch) ~> r }
  private implicit def keyword (kw: String): HParser[String] = (word(kw) <~ elem("identifier part", Character.isJavaIdentifierPart).!).^
  private implicit def symbol (ch: Char): HParser[Char] = elem(ch).^
}
