package phenan.prj

import java.io._

import phenan.prj.declaration.DeclarationCompiler
import phenan.prj.internal._
import phenan.prj.ir._
import phenan.prj.state.JState
import phenan.prj.typing._

class JCompiler (implicit val state: JState) {

  def compile (files: List[String]): Unit = {
    generateIR(files)
    generateClassFile()
  }

  def generateIR (files: List[String]): Unit = {
    for (file <- files) generateIR(file)
  }

  def generateIR (file: String): Unit = {
    declarationCompiler.compile(file).foreach(registerIR)
  }

  def generateIR (reader: Reader, file: String): Unit = {
    declarationCompiler.compile(reader, file).foreach(registerIR)
  }

  def registerIR (ir: IRFile): Unit = {
    for (module <- ir.modules) modules += (module.internalName -> module)
  }

  def findIR (name: String): Option[IRClass] = compiled.get(name).orElse(modules.get(name))

  def generateClassFile (): Unit = {
    while (modules.nonEmpty) {
      val (name, module) = modules.head
      generateClassFile(module)
      compiled += name -> module
      modules -= name
    }
  }

  private def generateClassFile (module: IRClass): Unit = {

  }

  val unifier = new Unifier(this)
  val classLoader: JClassLoader = new JClassLoaderImpl(this)
  val typeLoader: JTypeLoader = new JTypeLoaderImpl(this)
  val declarationCompiler = new DeclarationCompiler(this)

  private var modules: Map[String, IRClass] = Map.empty
  private var compiled: Map[String, IRClass] = Map.empty
}
