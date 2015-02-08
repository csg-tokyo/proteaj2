package phenan.prj

import java.io.Reader

import scala.util._

class JCompiler {
  def compile (in: Reader): Try[JClass] = Failure(new RuntimeException(""))


  def findCompiling (name: String): Option[JClass] = None
}
