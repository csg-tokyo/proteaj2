package phenan.prj.ir

import phenan.prj._

trait Environment {
  def highestPriority (expected: JType): Option[String]
  def nextPriority (expected: JType, priority: String): Option[String]
  def expressionOperators (expected: JType, priority: String): List[JSyntax]

  def defineLocal (localType: JType, name: String): Environment
  def modifyContext (expression: IRExpression): Environment
  def resolver: NameResolver

  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }
}

case class BaseEnvironment (file: IRFile) extends Environment {
  def highestPriority (expected: JType): Option[String] = ???
  def nextPriority (expected: JType, priority: String): Option[String] = ???
  def expressionOperators (expected: JType, priority: String): List[JSyntax] = ???

  def defineLocal (localType: JType, name: String): Environment = ???
  def modifyContext(expression: IRExpression): Environment = ???

  def resolver: NameResolver = file.resolver
}
