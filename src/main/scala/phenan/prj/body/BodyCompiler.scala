package phenan.prj.body

import phenan.prj._
import phenan.prj.ir._

import scala.util.Try

class BodyCompiler (compiler: JCompiler) {
  def expression (code: String, expected: JType, env: Environment): Try[IRExpression] = parsers.parse(parsers.ExpressionParsers(expected, env).expression, code)

  private val parsers = new BodyParsers(compiler)
}
