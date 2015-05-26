package phenan.prj.ir

import phenan.prj._

trait Environment {
  def highestPriority (expected: JType): Option[Priority]
  def nextPriority (expected: JType, priority: Priority): Option[Priority]
  def expressionOperators (expected: JType, priority: Priority): List[Syntax]

  def defineLocal (localType: JType, name: String): Environment
  def modifyContext (expression: IRExpression): Environment
  def resolver: NameResolver

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }
}

case class BaseEnvironment (resolver: NameResolver) extends Environment {
  def highestPriority (expected: JType): Option[Priority] = ???
  def nextPriority (expected: JType, priority: Priority): Option[Priority] = ???
  def expressionOperators (expected: JType, priority: Priority): List[Syntax] = ???

  def defineLocal (localType: JType, name: String): Environment = ???
  def modifyContext(expression: IRExpression): Environment = ???
}
