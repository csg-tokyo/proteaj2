package phenan.prj.decl

import java.io._

import phenan.prj.internal.JClassLoader
import phenan.prj.ir.IRFile
import phenan.prj.state.JState

import scala.util.Try

class DeclarationCompiler (loader: JClassLoader) (implicit state: JState) {
  def compile (reader: Reader): Try[IRFile] = DeclarationParser(reader).map(_.parseAll).flatMap(generator.run)

  val generator = new IRGenerator(loader)
}
