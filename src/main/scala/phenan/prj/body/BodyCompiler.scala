package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scala.util.Try

trait BodyCompiler {
  this: BodyParser with StatementParser with ExpressionParser with JTypeLoader with Environments with IRStatements with IRExpressions with JModules =>

  def parseMethodBody (code: String, expected: JType, env: Environment): Try[IRMethodBody] = BodyParsers.parse(BodyParsers.getStatementParsers(expected, env).methodBody, code)

  def parseConstructorBody (code: String, env: Environment): Try[IRConstructorBody] = BodyParsers.parse(BodyParsers.getStatementParsers(voidType, env).constructorBody, code)

  def parseInitializerBody (code: String, env: Environment): Try[IRInitializerBody] = BodyParsers.parse(BodyParsers.getStatementParsers(voidType, env).initializerBody, code)

  def parseExpression (code: String, expected: JType, env: Environment): Try[IRExpression] = BodyParsers.parse(BodyParsers.getExpressionParser(expected, env), code)
}
