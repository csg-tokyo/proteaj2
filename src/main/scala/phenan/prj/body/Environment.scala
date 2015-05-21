package phenan.prj.body

import phenan.prj.ir._

trait Environment {
  def defineLocals (local: IRLocalDeclaration): Environment
  def modifyContext (expressionStatement: IRExpressionStatement): Environment
  def resolver: NameResolver
}

case class BaseEnvironment (resolver: NameResolver) extends Environment {
  override def defineLocals(local: IRLocalDeclaration): Environment = ???
  override def modifyContext(expressionStatement: IRExpressionStatement): Environment = ???
}
