package phenan.prj.internal

case class ClassSignature (typeParams: List[FormalTypeParameter], superClass: ClassTypeSignature, interfaces: List[ClassTypeSignature])

case class MethodSignature (typeParams: List[FormalTypeParameter], paramTypes: List[TypeSignature], returnType: TypeSignature, throwTypes: List[TypeSignature])

case class FormalTypeParameter (name: String, classBound: Option[TypeSignature], interfaceBounds: List[TypeSignature])

sealed trait TypeSignature extends TypeArgument

sealed trait ClassTypeSignature extends TypeSignature

case class SimpleClassTypeSignature (clazz: String, args: List[TypeArgument]) extends ClassTypeSignature

case class MemberClassTypeSignature (outer: ClassTypeSignature, clazz: String, args: List[TypeArgument]) extends ClassTypeSignature

sealed trait PrimitiveTypeSignature extends TypeSignature

case object ByteTypeSignature extends PrimitiveTypeSignature
case object CharTypeSignature extends PrimitiveTypeSignature
case object DoubleTypeSignature extends PrimitiveTypeSignature
case object FloatTypeSignature extends PrimitiveTypeSignature
case object IntTypeSignature extends PrimitiveTypeSignature
case object LongTypeSignature extends PrimitiveTypeSignature
case object ShortTypeSignature extends PrimitiveTypeSignature
case object BoolTypeSignature extends PrimitiveTypeSignature
case object VoidTypeSignature extends PrimitiveTypeSignature

case class ArrayTypeSignature (component: TypeSignature) extends TypeSignature

case class TypeVariableSignature (name: String) extends TypeSignature

sealed trait TypeArgument

case class PureVariable (name: String) extends TypeArgument

case class UpperBoundWildcardArgument (signature: TypeSignature) extends TypeArgument

case class LowerBoundWildcardArgument (signature: TypeSignature) extends TypeArgument

object UnboundWildcardArgument extends TypeArgument
