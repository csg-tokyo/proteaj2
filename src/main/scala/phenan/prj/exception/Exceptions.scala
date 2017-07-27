package phenan.prj.exception

case class ClassFileNotFoundException (msg: String) extends Exception(msg)

case class InvalidClassFileException (msg: String) extends Exception(msg)

case class InvalidTypeException (msg: String, cause: Throwable) extends Exception(msg)

object InvalidTypeException {
  def apply (msg: String): InvalidTypeException = InvalidTypeException(msg, null)
}

case class ParseException (msg: String) extends Exception(msg)

case class InvalidASTException (msg: String, cause: Throwable) extends Exception(msg, cause)

object InvalidASTException {
  def apply (msg: String): InvalidASTException = InvalidASTException(msg, null)
}

case class InitializationFailedException (cause: Throwable) extends Exception(cause)
