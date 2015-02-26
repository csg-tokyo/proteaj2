package phenan.prj.exception

case class ParseException (expected: String, actual: String, file: String, line: Int)
  extends Exception("parse error : " + file + " : " + line + "\n  expected " + expected + ", but found " + actual)
