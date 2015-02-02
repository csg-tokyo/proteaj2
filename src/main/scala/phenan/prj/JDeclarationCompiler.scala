package phenan.prj

import phenan.prj.config._

import scala.util.Try

class JDeclarationCompiler (implicit config: JConfig) {
  def compile (name: String): Try[JClass] = ???
}
