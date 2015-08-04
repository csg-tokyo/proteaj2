package phenan.prj

import java.io._

import phenan.prj.body.BodyCompiler
import phenan.prj.declaration.DeclarationCompiler
import phenan.prj.generator.JavaCodeGenerators
import phenan.prj.internal._
import phenan.prj.ir._
import phenan.prj.state.JState
import phenan.prj.typing._

class JCompiler (implicit val state: JState) {

  def compile (files: List[String]): Unit = {
    generateIR(files)
    if (state.errors == 0) generateClassFile()
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

  val unifier = new Unifier(this)
  val classLoader: JClassLoader = new JClassLoaderImpl(this)
  val typeLoader: JTypeLoader = new JTypeLoader(this)
  val declarationCompiler = new DeclarationCompiler(this)
  val bodyCompiler = new BodyCompiler(this)

  private var modules: Map[String, IRModule] = Map.empty
  private var compiled: Map[String, IRModule] = Map.empty
}
