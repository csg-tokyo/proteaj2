package phenan.prj

sealed trait JOperatorSyntax {
  def priority: Option[String]
  def syntax: List[JSyntaxElement]
}

case class JExpressionSyntax (priority: Option[String], syntax: List[JSyntaxElement]) extends JOperatorSyntax
case class JLiteralSyntax (priority: Option[String], syntax: List[JSyntaxElement]) extends JOperatorSyntax
case class JStatementSyntax (priority: Option[String], syntax: List[JSyntaxElement]) extends JOperatorSyntax

sealed trait JSyntaxElement

case object JOperand extends JSyntaxElement
case object JOptionalOperand extends JSyntaxElement
case object JRepetition0 extends JSyntaxElement
case object JRepetition1 extends JSyntaxElement
case class JOperatorName (name: String) extends JSyntaxElement
case class JMetaValueRef (name: String) extends JSyntaxElement
case class JAndPredicate (sig: JParameterSignature) extends JSyntaxElement
case class JNotPredicate (sig: JParameterSignature) extends JSyntaxElement
