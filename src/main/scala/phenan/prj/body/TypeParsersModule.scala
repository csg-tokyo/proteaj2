package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

trait TypeParsersModule {
  this: CommonParsersModule with ContextSensitiveParsersModule with Environments with EnvModifyStrategy
    with NameResolvers with JTypeLoader with JClassLoader with JErasedTypes with JModules =>

  trait TypeParsers {
    this: CommonParsers with ContextSensitiveParsers =>

    def typeParsers: TypeParsersInterface = TypeParsersImpl

    trait TypeParsersInterface {
      def metaArguments: ContextFreeParser[List[MetaArgument]]

      def metaValue: ContextFreeParser[MetaArgument]

      def typeName: ContextFreeParser[JType]

      def componentType: ContextFreeParser[JType]

      def refType: ContextFreeParser[JRefType]

      def objectType: ContextFreeParser[JObjectType]

      def className: ContextFreeParser[JClass]

      def primitiveTypeName: ContextFreeParser[JPrimitiveType]

      def metaVariable: ContextFreeParser[MetaVariableRef]

      def wildcard: ContextFreeParser[JWildcard]
    }

    private object TypeParsersImpl extends TypeParsersInterface {
      lazy val metaArguments: ContextFreeParser[List[MetaArgument]] = ('<' ~> metaValue.+(',') <~ '>').? ^^ { _.getOrElse(Nil) }
      lazy val metaValue: ContextFreeParser[MetaArgument] = wildcard | metaVariable | refType
      lazy val typeName: ContextFreeParser[JType] = primitiveTypeName | refType
      lazy val componentType: ContextFreeParser[JType] = primitiveTypeName | objectType
      lazy val refType: ContextFreeParser[JRefType] = arrayType | typeVariable | objectType
      lazy val objectType: ContextFreeParser[JObjectType] = className ~ metaArguments ^^? {
        case clazz ~ args => getObjectType(clazz, args).toOption
      }
      lazy val packageName: ContextFreeParser[List[String]] = (identifier <~ '.').repFor[List[String]](Nil, isNotClassName, _ :+ _)

      lazy val className: ContextFreeParser[JClass] = innerClassName | topLevelClassName
      lazy val topLevelClassName: ContextFreeParser[JClass] = packageName >> { pack =>
        identifier ^^? { name => resolver.resolve(pack :+ name).toOption }
      }
      lazy val innerClassName: ContextFreeParser[JClass] = (className <~ '.') >> { name =>
        identifier ^^? { id => name.innerClasses.get(id).flatMap(loadClass_NoFail) }
      }

      lazy val typeVariable: ContextFreeParser[JTypeVariable] = identifier ^^? { name =>
        resolver.typeVariable(name)
      }
      lazy val metaVariable: ContextFreeParser[MetaVariableRef] = identifier ^^? { name =>
        resolver.metaVariable(name)
      }
      lazy val arrayType: ContextFreeParser[JArrayType] = typeName <~ emptyBracket ^^ { _.array }
      lazy val primitiveTypeName: ContextFreeParser[JPrimitiveType] = identifier ^? {
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
      lazy val wildcard: ContextFreeParser[JWildcard] = '?' ~> ("extends" ~> refType).? ~ ("super" ~> refType).? ^^ {
        case ub ~ lb => JWildcard(ub, lb)
      }

      private def isNotClassName (names: List[String]): Boolean = {
        rootResolver.isKnownPackage(names) || resolver.resolve(names).isFailure
      }
    }
  }
}