package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

trait Environment {
  def defineLocal (localType: JType, name: String): Environment
  def defineLocals (locals: IRLocalDeclaration): Environment = locals.declarators.foldLeft(this) { (e, d) =>
    e.defineLocal(locals.localType.array(d.dim), d.name)
  }
  def modifyContext (expressionStatement: IRExpressionStatement): Environment
  def resolver: NameResolver
}

case class BaseEnvironment (resolver: NameResolver) extends Environment {
  def defineLocal (localType: JType, name: String): Environment = ???
  def modifyContext(expressionStatement: IRExpressionStatement): Environment = ???
}
