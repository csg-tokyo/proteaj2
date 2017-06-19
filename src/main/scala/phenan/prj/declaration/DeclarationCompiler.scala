package phenan.prj.declaration

import java.io._

import phenan.prj.ir._

import scala.util.Try

trait DeclarationCompiler {
  this: IRs =>

  def compileDeclaration(file: String): Try[IRFile] = compileDeclaration(new FileReader(file), file)
  def compileDeclaration(file: File): Try[IRFile] = compileDeclaration(new FileReader(file), file.getPath)
  def compileDeclaration(reader: Reader, file: String): Try[IRFile] = {
    DeclarationParsers.tryParse(DeclarationParsers.compilationUnit, new BufferedReader(reader), file).map(cu => IRFile(cu, file))
  }
}
