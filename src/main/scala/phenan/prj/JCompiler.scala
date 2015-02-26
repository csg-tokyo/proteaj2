package phenan.prj

import java.io.Reader

import phenan.prj.decl.DeclarationCompiler
import phenan.prj.internal.JClassLoader
import phenan.prj.ir._
import phenan.prj.state.JState

import scala.util._

class JCompiler (implicit state: JState) {
  def generateIR (in: Reader, fileName: String): Unit = {
    val ir = declarationCompiler.compile(in, fileName)
    for (file <- ir; module <- file.modules) {
      modules += (module.internalName -> module)
    }
  }

  def findIR (name: String): Option[IRModule] = modules.get(name)

  private var modules: Map[String, IRModule] = Map.empty
  
  private val loader = new JClassLoader(this)
  private val declarationCompiler = new DeclarationCompiler(loader)
}
