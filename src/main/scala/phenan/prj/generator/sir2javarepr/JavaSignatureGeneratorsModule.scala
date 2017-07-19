package phenan.prj.generator.sir2javarepr

import phenan.prj._
import phenan.prj.exception._
import phenan.prj.generator.JavaRepr._
import phenan.util._

/**
  * Created by ichikawa on 2017/07/13.
  */
trait JavaSignatureGeneratorsModule {
  this: JModules =>

  object JavaSignatureGenerators {
    /* signatures */

    def typeToSig (t: JType): TypeSig = t match {
      case obj: JObjectType         => Union[TypeSig](objectType(obj))
      case prm: JPrimitiveType      => Union[TypeSig](PrimitiveSig(prm.name))
      case JArrayType(component)    => Union[TypeSig](ArraySig(typeToSig(component)))
      case JTypeVariable(name, _)   => Union[TypeSig](TypeVariableSig(name))
      case _: JCapturedWildcardType => throw InvalidASTException("captured wildcard is found in generated Java AST")
      case _: JUnboundTypeVariable  => throw InvalidASTException("unbound type variable is found in generated Java AST")
    }

    def objectType (obj: JObjectType): ClassSig = Union[ClassSig](topLevelClassObjectType(obj))

    def topLevelClassObjectType (obj: JObjectType): TopLevelClassSig = TopLevelClassSig (obj.erase.name, obj.erase.signature.metaParams.flatMap { param =>
      metaArgument(obj.env.getOrElse(param.name, throw InvalidASTException("invalid type argument")))
    })

    def metaArgument (arg: MetaArgument): Option[TypeArg] = arg match {
      case ref: JRefType   => Some(Union[TypeArg](typeToSig(ref)))
      case wild: JWildcard => Some(Union[TypeArg](wildcard(wild)))
      case _: MetaValue    => None
    }

    def wildcard (wild: JWildcard): Wildcard = wild match {
      case JWildcard(Some(ub), _) => Union[Wildcard](UpperBoundWildcard(typeToSig(ub)))
      case JWildcard(_, Some(lb)) => Union[Wildcard](LowerBoundWildcard(typeToSig(lb)))
      case JWildcard(None, None)  => Union[Wildcard](UnboundWildcard)
    }

    def typeParams (mps: List[FormalMetaParameter]): List[TypeParam] = mps.filter(_.metaType == JTypeSignature.typeTypeSig).map(typeParam)

    def typeParam (mp: FormalMetaParameter): TypeParam = TypeParam(mp.name, mp.bounds.map(typeSig))

    def typeSig (signature: JTypeSignature): TypeSig = signature match {
      case c: JClassTypeSignature        => Union[TypeSig](classSig(c))
      case p: JPrimitiveTypeSignature    => Union[TypeSig](primitiveSig(p))
      case JTypeVariableSignature(n)     => Union[TypeSig](TypeVariableSig(n))
      case JArrayTypeSignature(c)        => Union[TypeSig](ArraySig(typeSig(c)))
      case _: JCapturedWildcardSignature => throw InvalidASTException("captured wildcard is found in generated Java code")
    }

    def classSig (signature: JClassTypeSignature): ClassSig = signature match {
      case s: SimpleClassTypeSignature => Union[ClassSig](topLevelClassSig(s))
      case m: MemberClassTypeSignature => Union[ClassSig](memberClassSig(m))
    }

    def objectClassSig: ClassSig = classSig(JTypeSignature.objectTypeSig)

    def topLevelClassSig (signature: SimpleClassTypeSignature): TopLevelClassSig = TopLevelClassSig (signature.internalName.replace('/', '.').replace('$', '.'), signature.args.flatMap(typeArg))

    def memberClassSig (signature: MemberClassTypeSignature): MemberClassSig = MemberClassSig (classSig(signature.outer), signature.clazz, signature.args.flatMap(typeArg))

    def primitiveSig (signature: JPrimitiveTypeSignature): PrimitiveSig = signature match {
      case ByteTypeSignature   => PrimitiveSig("byte")
      case CharTypeSignature   => PrimitiveSig("char")
      case DoubleTypeSignature => PrimitiveSig("double")
      case FloatTypeSignature  => PrimitiveSig("float")
      case IntTypeSignature    => PrimitiveSig("int")
      case LongTypeSignature   => PrimitiveSig("long")
      case ShortTypeSignature  => PrimitiveSig("short")
      case BoolTypeSignature   => PrimitiveSig("boolean")
      case VoidTypeSignature   => PrimitiveSig("void")
    }

    def typeArg (arg: JTypeArgument): Option[TypeArg] = arg match {
      case signature: JTypeSignature => Some(Union[TypeArg](typeSig(signature)))
      case wild: WildcardArgument    => Some(Union[TypeArg](wildcard(wild)))
      case _: MetaVariableSignature  => None
    }

    def wildcard (wild: WildcardArgument): Wildcard = wild match {
      case WildcardArgument(Some(ub), _) => Union[Wildcard](UpperBoundWildcard(typeSig(ub)))
      case WildcardArgument(_, Some(lb)) => Union[Wildcard](LowerBoundWildcard(typeSig(lb)))
      case WildcardArgument(None, None)  => Union[Wildcard](UnboundWildcard)
    }
  }
}

