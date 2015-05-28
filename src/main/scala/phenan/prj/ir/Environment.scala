package phenan.prj.ir

import phenan.prj._

trait Environment {
  def highestPriority (expected: JType): Option[JPriority]
  def nextPriority (expected: JType, priority: JPriority): Option[JPriority]
  def expressionOperators (expected: JType, priority: JPriority): List[JSyntax]

  def defineLocal (localType: JType, name: String): Environment
  def modifyContext (expression: IRExpression): Environment
  def resolver: NameResolver

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }
}

case class BaseEnvironment (resolver: NameResolver) extends Environment {
  def highestPriority (expected: JType): Option[JPriority] = ???
  def nextPriority (expected: JType, priority: JPriority): Option[JPriority] = ???
  def expressionOperators (expected: JType, priority: JPriority): List[JSyntax] = ???

  def defineLocal (localType: JType, name: String): Environment = ???
  def modifyContext(expression: IRExpression): Environment = ???
}
