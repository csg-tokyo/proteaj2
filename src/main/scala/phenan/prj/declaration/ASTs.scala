package phenan.prj.declaration

import scala.util.parsing.input.Positional

case class CompilationUnit (header: Header, modules: List[ModuleDeclaration])

case class Header (pack: Option[PackageDeclaration], imports: List[ImportDeclaration])

case class PackageDeclaration (name: QualifiedName)

sealed trait ImportDeclaration

case class ClassImportDeclaration (name: QualifiedName) extends ImportDeclaration

case class PackageImportDeclaration (name: QualifiedName) extends ImportDeclaration

sealed trait StaticImportDeclaration extends ImportDeclaration

case class ImportStaticMemberDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class ImportStaticOnDemandDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class ImportDSLsDeclaration (dsls: List[QualifiedName], constraints: List[List[QualifiedName]]) extends ImportDeclaration

sealed trait ModuleDeclaration extends ClassMember with InterfaceMember with AnnotationMember

case class ClassDeclaration (modifiers: List[Modifier], name: String, metaParameters: List[MetaParameter], superClass: Option[TypeName], interfaces: List[TypeName], members: List[ClassMember]) extends ModuleDeclaration
case class EnumDeclaration (modifiers: List[Modifier], name: String, interfaces: List[TypeName], enumConstants: List[EnumConstant], members: List[ClassMember]) extends ModuleDeclaration
case class InterfaceDeclaration (modifiers: List[Modifier], name: String, metaParameters: List[MetaParameter], superInterfaces: List[TypeName], members: List[InterfaceMember]) extends ModuleDeclaration
case class AnnotationDeclaration (modifiers: List[Modifier], name: String, members: List[AnnotationMember]) extends ModuleDeclaration
case class DSLDeclaration (modifiers: List[Modifier], name: String, metaParameters: List[MetaParameter], withDSLs: List[QualifiedName], members: List[DSLMember]) extends ModuleDeclaration

sealed trait ClassMember
sealed trait InterfaceMember
sealed trait AnnotationMember
sealed trait DSLMember

case class InstanceInitializer (block: BlockSnippet) extends ClassMember
case class StaticInitializer (block: BlockSnippet) extends ClassMember
case class ConstructorDeclaration (modifiers: List[Modifier], metaParameters: List[MetaParameter], formalParameters: List[FormalParameter], clauses: List[MethodClause], body: BlockSnippet) extends ClassMember with DSLMember
case class FieldDeclaration (modifiers: List[Modifier], fieldType: TypeName, declarators: List[VariableDeclarator]) extends ClassMember with InterfaceMember with AnnotationMember with DSLMember
case class MethodDeclaration (modifiers: List[Modifier], metaParameters: List[MetaParameter], returnType: TypeName, name: String, formalParameters: List[FormalParameter], clauses: List[MethodClause], body: Option[BlockSnippet]) extends ClassMember with InterfaceMember
case class AnnotationElementDeclaration (modifiers: List[Modifier], elementType: TypeName, name: String, dim: Int, defaultValue: Option[AnnotationElement]) extends AnnotationMember
case class OperatorDeclaration (label: Option[String], modifiers: List[Modifier], metaParameters: List[MetaParameter], returnType: TypeName, returnBounds: List[TypeName], priority: Option[QualifiedName], syntax: List[SyntaxElement], formalParameters: List[FormalParameter], clauses: List[MethodClause], body: Option[BlockSnippet]) extends DSLMember
case class PrioritiesDeclaration (names: List[String], constraints: List[List[QualifiedName]]) extends DSLMember

case class FormalParameter (modifiers: List[Modifier], parameterType: ParameterType, varArgs: Boolean, name: String, dim: Int, initializer: Option[ExpressionSnippet], scopeFor: List[TypeName])
case class VariableDeclarator (name: String, dim: Int, initializer: Option[ExpressionSnippet])

sealed trait MethodClause
case class ThrowsClause (exceptions: List[TypeName]) extends MethodClause
case class ActivatesClause (contexts: List[TypeName]) extends MethodClause
case class DeactivatesClause (contexts: List[TypeName]) extends MethodClause
case class RequiresClause (contexts: List[TypeName]) extends MethodClause

case class EnumConstant (annotations: List[Annotation], name: String, arguments: List[ExpressionSnippet], members: List[ClassMember])

case class ExpressionSnippet (snippet: String) extends Positional
case class BlockSnippet (snippet: String) extends Positional

sealed trait SyntaxElement

case class OperatorName (name: String) extends SyntaxElement
case class RegexName (name: String) extends SyntaxElement
case class MetaValueRef (name: String, priority: Option[QualifiedName]) extends SyntaxElement
case class Operand (priority: Option[QualifiedName]) extends SyntaxElement
case class Repetition0 (priority: Option[QualifiedName]) extends SyntaxElement
case class Repetition1 (priority: Option[QualifiedName]) extends SyntaxElement
case class OptionalOperand (priority: Option[QualifiedName]) extends SyntaxElement
case class AndPredicate (prd: TypeName, priority: Option[QualifiedName]) extends SyntaxElement
case class NotPredicate (prd: TypeName, priority: Option[QualifiedName]) extends SyntaxElement

sealed trait Modifier

case object PublicModifier extends Modifier
case object PrivateModifier extends Modifier
case object ProtectedModifier extends Modifier
case object StaticModifier extends Modifier
case object FinalModifier extends Modifier
case object SynchronizedModifier extends Modifier
case object VolatileModifier extends Modifier
case object TransientModifier extends Modifier
case object NativeModifier extends Modifier
case object AbstractModifier extends Modifier
case object StrictFPModifier extends Modifier
case object PureModifier extends Modifier
case object LiteralModifier extends Modifier
case object ExactModifier extends Modifier

sealed trait AnnotationElement
sealed trait Annotation extends Modifier with AnnotationElement {
  def name: QualifiedName
}

case class FullAnnotation (name: QualifiedName, args: Map[String, AnnotationElement]) extends Annotation
case class SingleElementAnnotation (name: QualifiedName, arg: AnnotationElement) extends Annotation
case class MarkerAnnotation (name: QualifiedName) extends Annotation

case class ArrayOfAnnotationElement (array: List[AnnotationElement]) extends AnnotationElement
case class EnumConstantElement (name: List[String]) extends AnnotationElement

sealed trait AnnotationExpression extends AnnotationElement

/*
case class ConditionalExpression    (cond: AnnotationExpression, thenExpr: AnnotationExpression, elseExpr: AnnotationExpression) extends AnnotationExpression
case class ConditionalOrExpression  (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class ConditionalAndExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class InclusiveOrExpression    (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class ExclusiveOrExpression    (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class AndExpression            (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression

case class EqualsExpression    (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class NotEqualsExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression

case class LTRelationalExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class RTRelationalExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class LERelationalExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class RERelationalExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression

case class LeftShiftExpression         (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class RightShiftExpression        (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class LogicalRightShiftExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression

case class AddExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class SubExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class MulExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class DivExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression
case class ModExpression (left: AnnotationExpression, right: AnnotationExpression) extends AnnotationExpression

case class PreIncrementExpression (operand: AnnotationExpression) extends AnnotationExpression
case class PreDecrementExpression (operand: AnnotationExpression) extends AnnotationExpression
case class UnaryPlusExpression    (operand: AnnotationExpression) extends AnnotationExpression
case class UnaryMinusExpression   (operand: AnnotationExpression) extends AnnotationExpression
case class BitInversionExpression (operand: AnnotationExpression) extends AnnotationExpression
case class LogicalNotExpression   (operand: AnnotationExpression) extends AnnotationExpression

case class CastExpression (targetType: TypeName, operand: AnnotationExpression) extends AnnotationExpression

case class PostIncrementExpression (operand: AnnotationExpression) extends AnnotationExpression
case class PostDecrementExpression (operand: AnnotationExpression) extends AnnotationExpression

case class CharLiteralExpression (value: Char) extends AnnotationExpression
case class IntLiteralExpression (value: Int) extends AnnotationExpression
case class LongLiteralExpression (value: Long) extends AnnotationExpression
case class FloatLiteralExpression (value: Float) extends AnnotationExpression
case class DoubleLiteralExpression (value: Double) extends AnnotationExpression
*/

case class StringLiteralExpression (value: String) extends AnnotationExpression
case class ClassLiteralExpression (name: TypeName) extends AnnotationExpression

sealed trait MetaParameter {
  def name: String
}
case class TypeParameter (name: String, bounds: List[TypeName]) extends MetaParameter
case class MetaValueParameter (name: String, metaType: TypeName) extends MetaParameter

sealed trait ParameterType
case class ContextualType (contexts: List[TypeName], paramType: ParameterType) extends ParameterType

sealed trait TypeArgument
case class TypeName (name: QualifiedName, args: List[TypeArgument], dim: Int) extends TypeArgument with ParameterType
case class UpperBoundWildcardType (bound: TypeName) extends TypeArgument
case class LowerBoundWildcardType (bound: TypeName) extends TypeArgument
case object UnboundWildcardType extends TypeArgument

case class QualifiedName (names: List[String])
