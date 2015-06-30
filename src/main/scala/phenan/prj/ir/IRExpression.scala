package phenan.prj.ir

import phenan.prj._

import scala.util._

sealed trait IRExpression {
  def eval: Try[MetaValue] = ???
  def staticType: Option[JType] = ???
  def activates: List[IRContextRef] = ???
  def deactivates: List[IRContextRef] = ???
}

case class IRDSLOperation (method: JMethod, metaArgs: Map[String, MetaValue], args: List[IRExpression]) extends IRExpression

case class IRContextOperation (context: IRContextRef, method: JMethod, metaArgs: Map[String, MetaValue], args: List[IRExpression]) extends IRExpression

case class IRVariableArguments (args: List[IRExpression]) extends IRExpression

case class IRStaticMethodCall (method: JMethod, metaArgs: Map[String, MetaValue], args: List[IRExpression]) extends IRExpression

case class IRLocalVariableRef (localType: JType, name: String) extends IRExpression

case class IRContextRef (contextType: JObjectType, id: Int) extends IRExpression
