package phenan.prj.declaration

import java.io._

import phenan.prj.JCompiler
import phenan.prj.ir._

import scala.util.Try

class DeclarationCompiler (compiler: JCompiler) {
  import DeclarationParsers._

  def compile (file: String): Try[IRFile] = compile(new FileReader(file), file)
  def compile (reader: Reader, file: String): Try[IRFile] = tryParse(compilationUnit, new BufferedReader(reader), file).map(cu => IRFile(cu, file, root))

  val root: RootResolver = NameResolver.root(compiler)
}
