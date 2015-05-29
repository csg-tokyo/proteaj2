package phenan.prj.ir

import phenan.prj._

import scala.util.Try

sealed trait IRExpression {
  def eval: Try[MetaValue] = ???
}

case class IROperation (syntax: JSyntax, operands: List[IRExpression]) extends IRExpression

case class IRDefaultArgument (parameter: JParameter) extends IRExpression

case class IRVariableArguments (args: List[IRExpression]) extends IRExpression
