package phenan.prj

import java.io.{FileReader, Reader}

import phenan.prj.decl.DeclarationCompiler
import phenan.prj.internal.JClassLoader
import phenan.prj.ir._
import phenan.prj.state.JState

import scala.util._

class JCompiler (implicit state: JState) {

  def compile (files: List[String]): Unit = {
    for (file <- files) generateIR(new FileReader(file), file)

    while (modules.nonEmpty) {
      val (name, module) = modules.head
      generateClassFile(module)
      compiled += name -> module
      modules -= name
    }
  }

  def generateIR (in: Reader, fileName: String): Unit = {
    val ir = declarationCompiler.compile(in, fileName)
    for (file <- ir; module <- file.modules) {
      modules += (module.internalName -> module)
    }
  }

  def findIR (name: String): Option[IRModule] = compiled.get(name).orElse(modules.get(name))

  private def generateClassFile (module: IRModule): Unit = {

  }

  val loader = new JClassLoader(this)
  val declarationCompiler = new DeclarationCompiler(loader)

  private var modules: Map[String, IRModule] = Map.empty
  private var compiled: Map[String, IRModule] = Map.empty
}
