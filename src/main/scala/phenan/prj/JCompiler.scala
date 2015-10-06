package phenan.prj

import java.io._

import phenan.prj.body.BodyCompiler
import phenan.prj.declaration.DeclarationCompiler
import phenan.prj.generator.JavaClassFileGenerator
import phenan.prj.internal._
import phenan.prj.ir._
import phenan.prj.state._
import phenan.prj.typing._

import scala.collection.mutable

class JCompiler (val state: JState) {

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
    files += ir
    for (module <- ir.modules) modules += (module.internalName -> module)
  }

  def findIR (name: String): Option[IRModule] = modules.get(name)

  val unifier = new Unifier(this)
  val classLoader: JClassLoader = new JClassLoaderImpl(this)
  val typeLoader: JTypeLoader = new JTypeLoader(this)
  val declarationCompiler = new DeclarationCompiler(this)
  val bodyCompiler = new BodyCompiler(this)

  private val files: mutable.MutableList[IRFile] = mutable.MutableList.empty
  private var modules: Map[String, IRModule] = Map.empty
}

object JCompiler {
  def main (args: Array[String]): Unit = JConfig.parseCommandLineArgs(args).foreach { case (state, files) =>
    val compiler = new JCompiler(state)
    compiler.generateIR(files)
    JavaClassFileGenerator.compile(compiler.files.toList, compiler.state)
  }
}
