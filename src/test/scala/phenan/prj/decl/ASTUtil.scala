package phenan.prj.decl

trait ASTUtil {
  def packageDcl (names: String*): PackageDeclaration = PackageDeclaration(qualifiedName(names:_*))
  def classImport (names: String*): SingleClassImportDeclaration = SingleClassImportDeclaration(qualifiedName(names:_*))
  def packageImport (names: String*): PackageImportDeclaration = PackageImportDeclaration(qualifiedName(names:_*))
  def staticImport (names: String*): StaticMemberImportDeclaration = StaticMemberImportDeclaration(qualifiedName(names:_*))
  def staticImportAll (names: String*): AllStaticMembersImportDeclaration = AllStaticMembersImportDeclaration(qualifiedName(names:_*))
  def dslImport (names: String*): DSLImportDeclaration = DSLImportDeclaration(qualifiedName(names:_*), None)

  def arrayOf (components: AnnotationElement*): ArrayOfAnnotationElement = ArrayOfAnnotationElement(components.toList)
  def expression (src: String, line: Int): ExpressionSnippet = ExpressionSnippet(Snippet(src, line))
  def qualifiedName (names: String*): QualifiedName = QualifiedName(names.toList)

  implicit class DSLImportDclOps (d : DSLImportDeclaration) {
    def < (names: QualifiedName): DSLImportDeclaration = DSLImportDeclaration(d.name, d.precedence match {
      case Some(AscendingDSLPrecedence(p)) => Some(AscendingDSLPrecedence(p :+ names))
      case None => Some(AscendingDSLPrecedence(List(names)))
      case _    => throw new Exception("bad ast")
    })
    def > (names: QualifiedName): DSLImportDeclaration = DSLImportDeclaration(d.name, d.precedence match {
      case Some(DescendingDSLPrecedence(p)) => Some(DescendingDSLPrecedence(p :+ names))
      case None => Some(DescendingDSLPrecedence(List(names)))
      case _    => throw new Exception("bad ast")
    })
  }
}
