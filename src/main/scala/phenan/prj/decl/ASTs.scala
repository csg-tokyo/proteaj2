package phenan.prj.decl

case class CompilationUnit (header: Header, modules: List[ModuleDeclaration], fileName: String)

case class Header (pack: Option[PackageDeclaration], imports: List[ImportDeclaration])

case class PackageDeclaration (name: QualifiedName)

sealed trait ImportDeclaration

sealed trait ClassImportDeclaration extends ImportDeclaration

case class SingleClassImportDeclaration (name: QualifiedName) extends ClassImportDeclaration

case class PackageImportDeclaration (name: QualifiedName) extends ClassImportDeclaration

sealed trait StaticImportDeclaration extends ImportDeclaration

case class StaticMemberImportDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class AllStaticMembersImportDeclaration (name: QualifiedName) extends StaticImportDeclaration

case class DSLImportDeclaration (name: QualifiedName, precedence: Option[DSLPrecedence]) extends ImportDeclaration

sealed trait DSLPrecedence

case class AscendingDSLPrecedence (names: List[QualifiedName]) extends DSLPrecedence

case class DescendingDSLPrecedence (names: List[QualifiedName]) extends DSLPrecedence

sealed trait ModuleDeclaration extends ClassMember {
  def modifiers: List[Modifier]
  def name: String
}

case class ClassDeclaration (modifiers: List[Modifier], name: String, typeParameters: List[TypeParameter], superClass: Option[ClassTypeName], interfaces: List[ClassTypeName], members: List[ClassMember]) extends ModuleDeclaration
case class EnumDeclaration (modifiers: List[Modifier], name: String, interfaces: List[ClassTypeName], enumConstants: List[EnumConstant], members: List[ClassMember]) extends ModuleDeclaration
case class InterfaceDeclaration (modifiers: List[Modifier], name: String, typeParameters: List[TypeParameter], superInterfaces: List[ClassTypeName], members: List[ClassMember]) extends ModuleDeclaration
case class AnnotationDeclaration (modifiers: List[Modifier], name: String, members: List[AnnotationMember]) extends ModuleDeclaration
case class DSLDeclaration (modifiers: List[Modifier], name: String, withDSLs: Option[DependsDSLs], members: List[DSLMember]) extends ModuleDeclaration

sealed trait ClassMember
sealed trait AnnotationMember
sealed trait DSLMember
sealed trait ContextMember

case class InstanceInitializer (block: BlockSnippet) extends ClassMember
case class StaticInitializer (block: BlockSnippet) extends ClassMember
case class ConstructorDeclaration (modifiers: List[Modifier], typeParameters: List[TypeParameter], formalParameters: List[FormalParameter], throws: List[ClassTypeName], body: BlockSnippet) extends ClassMember with ContextMember
case class FieldDeclaration (modifiers: List[Modifier], fieldType: TypeName, declarators: List[VariableDeclarator]) extends ClassMember with DSLMember with ContextMember
case class MethodDeclaration (modifiers: List[Modifier], typeParameters: List[TypeParameter], returnType: TypeName, name: String, formalParameters: List[FormalParameter], throws: List[ClassTypeName], body: Option[BlockSnippet]) extends ClassMember
case class OperatorDeclaration (label: Option[String], modifiers: List[Modifier], typeParameters: List[TypeParameter], returnType: TypeName, syntax: List[SyntaxElement], formalParameters: List[FormalParameter], throws: List[ClassTypeName], body: Option[BlockSnippet]) extends DSLMember with ContextMember
case class PrioritiesDeclaration (names: List[String], constraints: List[Constraint]) extends DSLMember

case class ContextDeclaration (modifiers: List[Modifier], name: String, typeParameters: List[TypeParameter], superClass: Option[ClassTypeName], interfaces: List[ClassTypeName], members: List[ContextMember]) extends DSLMember

case class FormalParameter (modifiers: List[Modifier], parameterType: TypeName, varArgs: Boolean, name: String, dim: Int, initializer: Option[ExpressionSnippet])
case class VariableDeclarator (name: String, dim: Int, initializer: Option[ExpressionSnippet])

case class EnumConstant (annotations: List[Annotation], name: String, arguments: List[ExpressionSnippet], members: List[ClassMember])

sealed trait SyntaxElement


sealed trait Constraint

sealed trait DependsDSLs
case class AscendingOrder (names: List[QualifiedName]) extends DependsDSLs
case class DescendingOrder (names: List[QualifiedName]) extends DependsDSLs

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

sealed trait AnnotationElement
sealed trait Annotation extends Modifier with AnnotationElement

case class FullAnnotation (name: QualifiedName, args: Map[String, AnnotationElement]) extends Annotation
case class SingleElementAnnotation (name: QualifiedName, arg: AnnotationElement) extends Annotation
case class MarkerAnnotation (name: QualifiedName) extends Annotation

case class ArrayOfAnnotationElement (array: List[AnnotationElement]) extends AnnotationElement
case class ExpressionSnippet (snippet: Snippet) extends AnnotationElement

case class BlockSnippet (snippet: Snippet)

case class TypeParameter (name: String, bounds: List[ClassTypeName])

sealed trait TypeArgument
sealed trait TypeName extends TypeArgument

case class ClassTypeName (name: QualifiedName, args: List[TypeArgument]) extends TypeName
case class ArrayTypeName (component: TypeName) extends TypeName
case class WildcardType (upper: Option[TypeName], lower: Option[TypeName]) extends TypeArgument

case class QualifiedName (names: List[String])
