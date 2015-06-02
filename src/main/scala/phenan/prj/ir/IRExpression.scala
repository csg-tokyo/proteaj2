package phenan.prj.ir

import phenan.prj._

import scala.util._

sealed trait IRExpression {
  def eval: Try[MetaValue] = ???
}

case class IROperation (syntax: JSyntax, args: List[IRArgument]) extends IRExpression



sealed trait IRArgument

sealed trait IRRuntimeArgument extends IRArgument

case class IRNormalArgument (arg: IRExpression) extends IRRuntimeArgument

case class IROptionalArgument (operand: JOptionalOperand, arg: Option[IRExpression]) extends IRRuntimeArgument

case class IRVariableArguments (args: List[IRExpression]) extends IRRuntimeArgument

case class IRMetaArgument (operand: JMetaOperand, arg: IRExpression) extends IRArgument {
  def binding = arg.eval.map(operand.name -> _)
}