package phenan.prj.ir

import phenan.prj._

import scala.util._

sealed trait IRExpression {
  def eval: Try[MetaValue] = ???
  def activates: List[IRContextRef] = ???
  def deactivates: List[IRContextRef] = ???
}

case class IRDSLOperation (method: JMethod, args: List[IRArgument]) extends IRExpression

case class IRContextOperation (context: IRContextRef, method: JMethod, args: List[IRArgument]) extends IRExpression

case class IRLocalVariableRef (localType: JType, name: String) extends IRExpression

case class IRContextRef (contextType: JObjectType, id: Int) extends IRExpression

sealed trait IRArgument

sealed trait IRRuntimeArgument extends IRArgument

case class IRNormalArgument (arg: IRExpression) extends IRRuntimeArgument

case class IROptionalArgument (operand: JOptionalOperand, arg: Option[IRExpression]) extends IRRuntimeArgument

case class IRVariableArguments (args: List[IRExpression]) extends IRRuntimeArgument

case class IRMetaArgument (operand: JMetaOperand, arg: IRExpression) extends IRArgument {
  def binding = arg.eval.map(operand.name -> _)
}