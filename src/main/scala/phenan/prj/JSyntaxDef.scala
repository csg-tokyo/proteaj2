package phenan.prj

sealed trait JSyntaxDef {
  def priority: JPriority
  def syntax: List[JSyntaxElementDef]
}

case class JExpressionSyntaxDef (priority: JPriority, syntax: List[JSyntaxElementDef]) extends JSyntaxDef
case class JLiteralSyntaxDef (priority: JPriority, syntax: List[JSyntaxElementDef]) extends JSyntaxDef
case class JStatementSyntaxDef (priority: JPriority, syntax: List[JSyntaxElementDef]) extends JSyntaxDef

sealed trait JSyntaxElementDef

sealed trait JHoleDef extends JSyntaxElementDef
sealed trait JPredicateDef extends JSyntaxElementDef

case class JOperandDef (priority: Option[JPriority]) extends JHoleDef
case class JOptionalOperandDef (priority: Option[JPriority]) extends JHoleDef
case class JRepetition0Def (priority: Option[JPriority]) extends JHoleDef
case class JRepetition1Def (priority: Option[JPriority]) extends JHoleDef
case class JRegexNameDef (name: String) extends JHoleDef
case class JOperatorNameDef (name: String) extends JSyntaxElementDef
case class JMetaValueRefDef (name: String, priority: Option[JPriority]) extends JSyntaxElementDef
case class JAndPredicateDef (sig: JTypeSignature, priority: Option[JPriority]) extends JPredicateDef
case class JNotPredicateDef (sig: JTypeSignature, priority: Option[JPriority]) extends JPredicateDef

case class JPriority (clazz: JClassTypeSignature, name: String)