package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

trait Environment {
  def defineLocals (local: IRLocalDeclaration): Environment
  def modifyContext (expressionStatement: IRExpressionStatement): Environment
  def typeEnv: TypeEnvironment
  def resolver: NameResolver = typeEnv.resolver
}

trait TypeEnvironment {
  def typeVariable (name: String): Option[JTypeVariable]
  def metaVariable (name: String): Option[PureVariableRef]
  def resolver: NameResolver
}
