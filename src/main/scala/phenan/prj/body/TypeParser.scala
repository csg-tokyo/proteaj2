package phenan.prj.body

import phenan.prj._
import phenan.prj.ir.NameResolvers

import scalaz.Memo._

trait TypeParser {
  this: NameResolvers with JTypeLoader with JClassLoader with JErasedTypes with JModules =>

  trait TypeParsers {
    this: CommonParsers =>

    def getTypeParsers(resolver: NameResolver): TypeParsersInterface = cached(resolver)

    trait TypeParsersInterface {
      def metaArguments: HParser[List[MetaArgument]]

      def metaValue: HParser[MetaArgument]

      def typeName: HParser[JType]

      def componentType: HParser[JType]

      def refType: HParser[JRefType]

      def objectType: HParser[JObjectType]

      def className: HParser[JClass]

      def primitiveTypeName: HParser[JPrimitiveType]

      def metaVariable: HParser[MetaVariableRef]

      def wildcard: HParser[JWildcard]
    }

    private val cached: NameResolver => TypeParsersInterface = mutableHashMapMemo(new TypeParsersImpl(_))

    private class TypeParsersImpl(resolver: NameResolver) extends TypeParsersInterface {
      lazy val metaArguments: HParser[List[MetaArgument]] = ('<' ~> metaValue.+(',') <~ '>').? ^^ {
        _.getOrElse(Nil)
      }
      lazy val metaValue: HParser[MetaArgument] = wildcard | metaVariable | refType
      lazy val typeName: HParser[JType] = primitiveTypeName | refType
      lazy val componentType: HParser[JType] = primitiveTypeName | objectType
      lazy val refType: HParser[JRefType] = arrayType | typeVariable | objectType
      lazy val objectType: HParser[JObjectType] = className ~ metaArguments ^^? {
        case clazz ~ args => getObjectType(clazz, args)
      }
      lazy val packageName: HParser[List[String]] = (identifier <~ '.').*! { names =>
        !rootResolver.isKnownPackage(names) && resolver.resolve(names).isSuccess
      }
      lazy val className: HParser[JClass] = HParser.ref(innerClassName | topLevelClassName)
      lazy val topLevelClassName: HParser[JClass] = packageName ~ identifier ^^? {
        case pack ~ name => resolver.resolve(pack :+ name).toOption
      }
      lazy val innerClassName: HParser[JClass] = className ~ ('.' ~> identifier) ^^? {
        case name ~ id => name.innerClasses.get(id).flatMap(loadClass_NoFail)
      }
      lazy val typeVariable: HParser[JTypeVariable] = identifier ^^? resolver.typeVariable
      lazy val metaVariable: HParser[MetaVariableRef] = identifier ^^? resolver.metaVariable
      lazy val arrayType: HParser[JArrayType] = typeName <~ emptyBracket ^^ {
        _.array
      }
      lazy val primitiveTypeName: HParser[JPrimitiveType] = identifier ^? {
        case "byte"    => byteType
        case "char"    => charType
        case "double"  => doubleType
        case "float"   => floatType
        case "int"     => intType
        case "long"    => longType
        case "short"   => shortType
        case "boolean" => booleanType
        case "void"    => voidType
      }
      lazy val wildcard: HParser[JWildcard] = '?' ~> ("extends" ~> refType).? ~ ("super" ~> refType).? ^^ {
        case ub ~ lb => JWildcard(ub, lb)
      }
    }
  }
}