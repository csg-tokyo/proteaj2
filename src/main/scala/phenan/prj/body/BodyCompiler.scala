package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scala.util.Try

class BodyCompiler (compiler: JCompiler) {
  def methodBody (code: String, expected: JType, env: Environment): Try[IRMethodBody] = parsers.parse(parsers.StatementParsers(expected, env).methodBody, code)

  def constructorBody (code: String, env: Environment): Try[IRConstructorBody] = parsers.parse(parsers.StatementParsers(compiler.typeLoader.void, env).constructorBody, code)

  def initializerBody (code: String, env: Environment): Try[IRInitializerBody] = parsers.parse(parsers.StatementParsers(compiler.typeLoader.void, env).initializerBody, code)

  def expression (code: String, expected: JType, env: Environment): Try[IRExpression] = parsers.parse(parsers.getExpressionParser(expected, env), code)

  private val parsers = new BodyParsers(compiler)
}
