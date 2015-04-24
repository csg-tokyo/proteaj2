package phenan.prj.exception

case class ParseException (msg: String) extends Exception(msg)

object ParseException {
  def apply (expected: String, actual: String, file: String, line: Int): ParseException = {
    new ParseException("parse error : " + file + " : " + line + "\n  expected " + expected + ", but found " + actual)
  }
}
