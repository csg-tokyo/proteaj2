package phenan.prj

sealed trait JOperatorSyntaxDef {
  def priority: Option[String]
  def syntax: List[JSyntaxElementDef]
}

case class JExpressionSyntaxDef (priority: Option[String], syntax: List[JSyntaxElementDef]) extends JOperatorSyntaxDef
case class JLiteralSyntaxDef (priority: Option[String], syntax: List[JSyntaxElementDef]) extends JOperatorSyntaxDef
case class JStatementSyntaxDef (priority: Option[String], syntax: List[JSyntaxElementDef]) extends JOperatorSyntaxDef

sealed trait JSyntaxElementDef

sealed trait JHoleDef extends JSyntaxElementDef
sealed trait JPredicateDef extends JSyntaxElementDef

case object JOperandDef extends JHoleDef
case object JOptionalOperandDef extends JHoleDef
case object JRepetition0Def extends JHoleDef
case object JRepetition1Def extends JHoleDef
case class JOperatorNameDef (name: String) extends JSyntaxElementDef
case class JMetaValueRefDef (name: String) extends JSyntaxElementDef
case class JAndPredicateDef (sig: JParameterSignature) extends JPredicateDef
case class JNotPredicateDef (sig: JParameterSignature) extends JPredicateDef
