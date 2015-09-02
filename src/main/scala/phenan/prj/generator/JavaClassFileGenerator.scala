package phenan.prj.generator

import java.io.File
import java.net.URI
import javax.tools._

import phenan.prj.ir.IRFile
import phenan.prj.state.JState

import scala.collection.JavaConversions._
import scala.util.control.Exception._

object JavaClassFileGenerator {
  def compile (files: List[IRFile], state: JState) = {
    val compiler = ToolProvider.getSystemJavaCompiler
    if (compiler == null) {
      state.error("Java Compiler cannot be found")
    }
    else {
      val compileOptions = List("-d", state.destination, "-cp", state.classPath)

      (allCatch opt files.map(JavaSourceObject(_))).fold { state.error("compile failed") } { compilationUnits =>
        val res = compiler.getTask(null, null, null, compileOptions, null, compilationUnits).call()
        state.info("compile status : " + res)
      }
    }
  }
}

class JavaSourceObject private (uri: URI, src: String) extends SimpleJavaFileObject (uri, JavaFileObject.Kind.SOURCE) {
  override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = src
}

object JavaSourceObject {
  def apply (file: IRFile): JavaSourceObject = try {
    val uri = getURI(file.filePath)
    val src = JavaReprGenerator.javaFile(file)
    new JavaSourceObject(uri, JavaCodeGenerators.javaFile(src))
  } catch {
    case e: Exception  =>
      e.printStackTrace()
      throw e
  }
  private def getURI (path: String): URI = {
    val absolutePath = new File(path).getAbsolutePath
    if (absolutePath.endsWith(".java")) URI.create("string://" + absolutePath)
    else {
      val dot = absolutePath.lastIndexOf('.')
      if (dot == -1) URI.create("string://" + absolutePath + JavaFileObject.Kind.SOURCE.extension)
      else URI.create("string://" + absolutePath.substring(0, dot) + JavaFileObject.Kind.SOURCE.extension)
    }
  }
}