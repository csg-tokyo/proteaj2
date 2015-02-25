package phenan.prj

import java.io.Reader

import phenan.prj.ir._
import phenan.prj.state.JState

import scala.util._

class JCompiler (implicit state: JState) {
  def compile (in: Reader): Try[IRClass] = Failure(new RuntimeException(""))


  def findIR (name: String): Option[IRClass] = None
}
