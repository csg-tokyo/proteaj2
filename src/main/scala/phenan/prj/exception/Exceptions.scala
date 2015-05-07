package phenan.prj.exception

case class ClassFileNotFoundException (msg: String) extends Exception(msg)

case class InvalidClassFileException (msg: String) extends Exception(msg)

case class InvalidTypeException (msg: String) extends Exception(msg)

case class ParseException (msg: String) extends Exception(msg)

object ParseException {
  @Deprecated
  def apply (expected: String, actual: String, file: String, line: Int): ParseException = {
    new ParseException("parse error : " + file + " : " + line + "\n  expected " + expected + ", but found " + actual)
  }
}