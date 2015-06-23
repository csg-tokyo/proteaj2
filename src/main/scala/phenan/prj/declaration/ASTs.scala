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

case class ImportStaticStarDeclaration (name: QualifiedName) extends StaticImportDeclaration

// ascending order
case class ImportDSLsDeclaration (dsls: List[QualifiedName]) extends ImportDeclaration

sealed trait ModuleDeclaration extends ClassMember with InterfaceMember with AnnotationMember

case class ClassDeclaration (modifiers: List[Modifier], name: String, metaParameters: List[MetaParameter], superClass: Option[TypeName], interfaces: List[TypeName], members: List[ClassMember]) extends ModuleDeclaration
case class EnumDeclaration (modifiers: List[Modifier], name: String, interfaces: List[TypeName], enumConstants: List[EnumConstant], members: List[ClassMember]) extends ModuleDeclaration
case class InterfaceDeclaration (modifiers: List[Modifier], name: String, metaParameters: List[MetaParameter], superInterfaces: List[TypeName], members: List[InterfaceMember]) extends ModuleDeclaration
case class AnnotationDeclaration (modifiers: List[Modifier], name: String, members: List[AnnotationMember]) extends ModuleDeclaration
case class DSLDeclaration (modifiers: List[Modifier], name: String, withDSLs: List[QualifiedName], members: List[DSLMember]) extends ModuleDeclaration

sealed trait ClassMember
sealed trait InterfaceMember
sealed trait AnnotationMember
sealed trait DSLMember
sealed trait ContextMember

case class InstanceInitializer (block: BlockSnippet) extends ClassMember
case class StaticInitializer (block: BlockSnippet) extends ClassMember
case class ConstructorDeclaration (modifiers: List[Modifier], metaParameters: List[MetaParameter], formalParameters: List[FormalParameter], clauses: List[MethodClause], body: BlockSnippet) extends ClassMember with ContextMember
case class FieldDeclaration (modifiers: List[Modifier], fieldType: TypeName, declarators: List[VariableDeclarator]) extends ClassMember with InterfaceMember with AnnotationMember with DSLMember with ContextMember
case class MethodDeclaration (modifiers: List[Modifier], metaParameters: List[MetaParameter], returnType: TypeName, name: String, formalParameters: List[FormalParameter], clauses: List[MethodClause], body: Option[BlockSnippet]) extends ClassMember with InterfaceMember
case class AnnotationElementDeclaration (modifiers: List[Modifier], elementType: TypeName, name: String, dim: Int, defaultValue: Option[AnnotationElement]) extends AnnotationMember
case class OperatorDeclaration (label: Option[String], modifiers: List[Modifier], metaParameters: List[MetaParameter], returnType: TypeName, priority: Option[QualifiedName], syntax: List[SyntaxElement], formalParameters: List[FormalParameter], clauses: List[MethodClause], body: Option[BlockSnippet]) extends DSLMember with ContextMember
case class PrioritiesDeclaration (names: List[String], constraints: List[List[QualifiedName]]) extends DSLMember

case class ContextDeclaration (modifiers: List[Modifier], name: String, metaParameters: List[MetaParameter], members: List[ContextMember]) extends DSLMember

case class FormalParameter (modifiers: List[Modifier], parameterType: ParameterType, priority: Option[QualifiedName], varArgs: Boolean, name: String, dim: Int, initializer: Option[ExpressionSnippet])
case class VariableDeclarator (name: String, dim: Int, initializer: Option[ExpressionSnippet])

sealed trait MethodClause
case class ThrowsClause (exceptions: List[TypeName]) extends MethodClause
case class ActivatesClause (contexts: List[TypeName]) extends MethodClause
case class DeactivatesClause (contexts: List[TypeName]) extends MethodClause
case class RequiresClause (contexts: List[TypeName]) extends MethodClause

case class EnumConstant (annotations: List[Annotation], name: String, arguments: List[ExpressionSnippet], members: List[ClassMember])

sealed trait SyntaxElement

case class OperatorName (name: String) extends SyntaxElement
case class MetaValueRef (name: String) extends SyntaxElement
case object Operand extends SyntaxElement
case object Repetition0 extends SyntaxElement
case object Repetition1 extends SyntaxElement
case object OptionalOperand extends SyntaxElement
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

sealed trait AnnotationElement
sealed trait Annotation extends Modifier with AnnotationElement {
  def name: QualifiedName
}

case class FullAnnotation (name: QualifiedName, args: Map[String, AnnotationElement]) extends Annotation
case class SingleElementAnnotation (name: QualifiedName, arg: AnnotationElement) extends Annotation
case class MarkerAnnotation (name: QualifiedName) extends Annotation

case class ArrayOfAnnotationElement (array: List[AnnotationElement]) extends AnnotationElement

case class ExpressionSnippet (snippet: String) extends AnnotationElement with Positional

case class BlockSnippet (snippet: String) extends Positional

sealed trait MetaParameter {
  def name: String
}
case class TypeParameter (name: String, bounds: List[TypeName]) extends MetaParameter
case class MetaValueParameter (name: String, metaType: TypeName, priority: Option[QualifiedName]) extends MetaParameter

sealed trait ParameterType
case class ContextualType (context: TypeName, paramType: ParameterType) extends ParameterType

sealed trait TypeArgument
case class TypeName (name: QualifiedName, args: List[TypeArgument], dim: Int) extends TypeArgument with ParameterType
case class UpperBoundWildcardType (bound: TypeName) extends TypeArgument
case class LowerBoundWildcardType (bound: TypeName) extends TypeArgument
case object UnboundWildcardType extends TypeArgument

case class QualifiedName (names: List[String])
