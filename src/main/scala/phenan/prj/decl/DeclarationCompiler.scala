package phenan.prj.decl

import java.io._

import phenan.prj.internal.JClassLoader
import phenan.prj.ir.IRFile
import phenan.prj.state.JState

import scala.util.Try

class DeclarationCompiler (loader: JClassLoader) (implicit state: JState) {
  def compile (reader: Reader, fileName: String): Try[IRFile] = DeclarationParser(reader, fileName).map(_.parseAll).map(new IRFile(_, loader))
}
