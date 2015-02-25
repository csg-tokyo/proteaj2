package phenan.prj.decl

import phenan.prj.internal.JClassLoader
import phenan.prj.ir._
import phenan.prj.state.JState

import scala.util._

class IRGenerator (loader: JClassLoader) (implicit state: JState) {
  def run (ast: CompilationUnit): Try[IRFile] = {
    ???
  }
}
