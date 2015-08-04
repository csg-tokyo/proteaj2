package phenan.prj.generator

import phenan.prj._

import phenan.util._

object JavaRepr {

  case class JavaFile (packageName: Option[String], modules: List[ModuleDef])

  type ModuleDef = ClassDef :|: EnumDef :|: InterfaceDef :|: UNil

  type ClassMember = FieldDef :|: MethodDef :|: ConstructorDef :|: InstanceInitializerDef :|: StaticInitializerDef :|: ModuleDef :|: UNil

  trait ClassDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def typeParameters: List[TypeParam]
    def superType: ClassSig
    def interfaces: List[ClassSig]
    def members: List[ClassMember]
  }

  trait EnumDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def interfaces: List[ClassSig]
    def constants: List[EnumConstantDef]
    def members: List[ClassMember]
  }

  trait InterfaceDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def name: String
    def typeParameters: List[TypeParam]
    def superInterfaces: List[ClassSig]
    def members: List[ClassMember]
  }

  trait FieldDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def fieldType: TypeSig
    def name: String
    def initializer: Option[Expression]
  }

  trait MethodDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def typeParameters: List[TypeParam]
    def returnType: TypeSig
    def name: String
    def parameters: List[Param]
    def throws: List[TypeSig]
    def body: Option[Block]
  }

  trait ConstructorDef {
    def annotations: List[JavaAnnotation]
    def modifiers: JModifier
    def typeParameters: List[TypeParam]
    def className: String
    def parameters: List[Param]
    def throws: List[TypeSig]
    def body: Block
  }

  case class InstanceInitializerDef (body: Block)

  case class StaticInitializerDef (body: Block)

  case class EnumConstantDef (name: String)

  case class Param (parameterType: TypeSig, name: String)

  type Statement = Block :|: LocalDeclarationStatement :|: IfStatement :|: WhileStatement :|: ForStatement :|: ReturnStatement :|: ExpressionStatement :|: ExplicitConstructorCall :|: UNil

  case class LocalDeclarationStatement (declaration: LocalDeclaration)
  
  case class LocalDeclaration (localType: TypeSig, declarators: List[LocalDeclarator])

  case class LocalDeclarator (name: String, dim: Int, initializer: Option[Expression])

  case class IfStatement (condition: Expression, thenStatement: Statement, elseStatement: Option[Statement])

  case class WhileStatement (condition: Expression, loopBody: Statement)

  type ForStatement = NormalForStatement :|: EnhancedForStatement :|: UNil

  type ForInit = LocalDeclaration :|: List[Expression] :|: UNil

  case class NormalForStatement (forInit: ForInit, condition: Option[Expression], update: List[Expression], loopBody: Statement)

  case class EnhancedForStatement (elementType: TypeSig, name: String, dim: Int, iterable: Expression, loopBody: Statement)

  case class ReturnStatement (returnValue: Expression)

  case class Block (statements: List[Statement])

  case class ExpressionStatement (statementExpression: Expression)

  type ExplicitConstructorCall = ThisConstructorCall :|: SuperConstructorCall :|: UNil

  case class ThisConstructorCall (typeArguments: List[TypeArg], arguments: List[Expression])

  case class SuperConstructorCall (typeArguments: List[TypeArg], arguments: List[Expression])

  type Expression = Assignment :|: MethodCall :|: FieldAccess :|: CastExpression :|: ArrayAccess :|: NewExpression :|: AnonymousClass :|: NewArray :|: ArrayInit :|: LocalRef :|: ThisRef :|: JavaLiteral :|: UNil

  type Receiver = Expression :|: ClassRef :|: SuperRef :|: UNil

  type Assignment = SimpleAssignment

  case class SimpleAssignment (left: Expression, right: Expression)

  case class MethodCall (receiver: Receiver, typeArguments: List[TypeArg], methodName: String, arguments: List[Expression])

  case class FieldAccess (receiver: Receiver, fieldName: String)

  case class CastExpression (destType: TypeSig, castedExpression: Expression)

  case class ArrayAccess (array: Expression, index: Expression)

  case class NewExpression (typeArguments: List[TypeArg], constructType: ClassSig, arguments: List[Expression])

  case class AnonymousClass (baseType: ClassSig, arguments: List[Expression], members: List[ClassMember])

  case class NewArray (componentType: TypeSig, arraySize: List[Expression], dim: Int)

  case class ArrayInit (componentType: TypeSig, dim: Int, components: List[Expression])

  case class LocalRef (name: String)

  case class ClassRef (name: String)

  case class SuperRef (thisType: ClassSig)

  case class ThisRef (thisType: ClassSig)

  type JavaLiteral = ClassLiteral :|: Literal[String] :|: Literal[Char] :|: Literal[Int] :|: Literal[Long] :|: Literal[Boolean] :|: UNil

  case class ClassLiteral (className: String, dim: Int)

  case class Literal[T] (value: T)

  case class JavaAnnotation (name: String, arguments: Map[String, AnnotationElement])

  type AnnotationElement = ElementArray :|: JavaAnnotation :|: JavaLiteral :|: EnumConstRef :|: UNil

  case class ElementArray (elements: List[AnnotationElement])

  case class EnumConstRef (enumName: String, constantName: String)

  case class TypeParam (name: String, bounds: List[TypeSig])

  type TypeArg = TypeSig :|: Wildcard :|: UNil

  type Wildcard = UnboundWildcard.type :|: UpperBoundWildcard :|: LowerBoundWildcard :|: UNil

  type TypeSig = ClassSig :|: ArraySig :|: TypeVariableSig :|: PrimitiveSig :|: UNil

  type ClassSig = TopLevelClassSig :|: MemberClassSig :|: UNil

  case class TopLevelClassSig (className: String, typeArguments: List[TypeArg])

  case class MemberClassSig (outer: ClassSig, className: String, typeArguments: List[TypeArg])

  case class ArraySig (component: TypeSig)

  case class TypeVariableSig (name: String)

  case class PrimitiveSig (name: String)

  object UnboundWildcard

  case class UpperBoundWildcard (bound: TypeSig)

  case class LowerBoundWildcard (bound: TypeSig)
}

