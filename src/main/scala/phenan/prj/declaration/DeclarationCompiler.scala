package phenan.prj.declaration

import java.io._

import phenan.prj.JCompiler
import phenan.prj.ir._

import scala.collection.immutable.PagedSeq
import scala.util.Try

class DeclarationCompiler (compiler: JCompiler) {
  import DeclarationParsers._

  def compile (file: String): Try[IRFile] = compile(PagedSeq.fromFile(file), file)
  def compile (reader: Reader, file: String): Try[IRFile] = compile(PagedSeq.fromReader(reader), file)
  def compile (seq: PagedSeq[Char], file: String): Try[IRFile] = tryParse(compilationUnit, seq, file).map(cu => new IRFile(cu, file, root))

  val root = NameResolver.root(compiler)
}
