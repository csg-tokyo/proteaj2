package phenan.prj

import phenan.prj.config._

import scala.util._

class JDeclarationCompiler (config: JConfig) {
  def compile (name: String): Try[JClass] = Failure(new RuntimeException(""))
}
