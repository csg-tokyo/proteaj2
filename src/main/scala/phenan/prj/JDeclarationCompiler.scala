package phenan.prj

import java.io.Reader

import scala.util._

class JDeclarationCompiler {
  def compile (in: Reader): Try[JClass] = Failure(new RuntimeException(""))


  def findCompiledClass (name: String): Option[JClass] = ???
}
