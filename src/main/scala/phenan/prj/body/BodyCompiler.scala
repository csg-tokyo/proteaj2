package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scala.util.Try

trait BodyCompiler {
  this: BodyParser with StatementParsersModule with ExpressionParsersModule with ContextSensitiveParsersModule with JTypeLoader with Environments with IRStatements with IRExpressions with JModules =>

  def parseMethodBody (code: String, expected: JType, env: ProcedureEnvironment): Try[IRMethodBody] = {
    new BodyParsers(env.resolver).getStatementParsers(expected).methodBody(code, env)
  }

  def parseConstructorBody (code: String, env: ProcedureEnvironment): Try[IRConstructorBody] = {
    new BodyParsers(env.resolver).getStatementParsers(voidType).constructorBody(code, env)
  }

  def parseInitializerBody (code: String, env: ProcedureEnvironment): Try[IRInitializerBody] = {
    new BodyParsers(env.resolver).getStatementParsers(voidType).initializerBody(code, env)
  }

  def parseExpression (code: String, expected: JType, env: ModuleEnvironment): Try[IRExpression] = {
    new BodyParsers(env.resolver).getExpressionParser(expected)(code, env)
  }
}
