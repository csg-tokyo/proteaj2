package phenan.prj

import java.io._

import phenan.prj.decl.DeclarationCompiler
import phenan.prj.internal._
import phenan.prj.ir._
import phenan.prj.state.JState

import scala.util._

class JCompiler (implicit state: JState) {

  def compile (files: List[String]): Unit = {
    generateIR(files)
    generateClassFile()
  }

  def generateIR (files: List[String]): Unit = {
    for (file <- files) generateIR(new FileReader(file), file)
  }

  def generateIR (in: Reader, fileName: String): Unit = {
    val ir = declarationCompiler.compile(in, fileName)
    for (file <- ir; module <- file.modules) {
      modules += (module.internalName -> module)
    }
  }

  def findIR (name: String): Option[IRModule] = compiled.get(name).orElse(modules.get(name))

  def generateClassFile (): Unit = {
    while (modules.nonEmpty) {
      val (name, module) = modules.head
      generateClassFile(module)
      compiled += name -> module
      modules -= name
    }
  }

  private def generateClassFile (module: IRModule): Unit = {

  }

  val classLoader: JClassLoader = new JClassLoaderImpl(this)
  val typeLoader: JTypeLoader = new JTypePool(this)
  val declarationCompiler = new DeclarationCompiler(this)

  private var modules: Map[String, IRModule] = Map.empty
  private var compiled: Map[String, IRModule] = Map.empty
}
