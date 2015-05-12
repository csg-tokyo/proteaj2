package phenan.prj.body

import phenan.prj.JType
import phenan.prj.combinator._

object BodyParsers extends TwoLevelParsers {
  type Elem = Char

  case class MethodBodyParsers (returnType: JType) {

  }

  def delimiter: LParser[Any] = elem("white space", Character.isWhitespace).*
}
