package phenan.prj.generator

import java.io.File
import java.net.URI
import javax.tools._

import phenan.prj.Application
import phenan.prj.generator.ir2sir.SimplifiedIRGeneratorsModule
import phenan.prj.generator.sir2javarepr.JavaReprGeneratorsModule
import phenan.prj.ir._

import scala.collection.JavaConverters._

trait JavaClassFileGenerator {
  this: JavaReprGeneratorsModule with SimplifiedIRGeneratorsModule with SimplifiedIRs with IRs with Application =>

  def generateClassFile (files: List[IRFile]): Unit = {
    val compiler = ToolProvider.getSystemJavaCompiler
    if (compiler == null) error("Java Compiler cannot be found")
    else {
      val compileOptions = List("-d", config.destination.getAbsolutePath, "-cp", config.getClassPathString, "-Xlint:unchecked")
      val compilationUnits = files.map(SimplifiedIRGenerators.compilationUnit).flatMap(createJavaSourceObject)
      if (config.displayJavaSources) compilationUnits.foreach(source => println(source.getSource))
      val res = compiler.getTask(null, null, null, compileOptions.asJava, null, compilationUnits.asJava).call()
      info("compile status : " + res)
    }
  }

  private def createJavaSourceObject (sir: SIRFile): Option[JavaSourceObject] = try {
    val uri = getURI(sir.filePath)
    val src = JavaReprGenerators.javaFile(sir)
    Some(new JavaSourceObject(uri, JavaCodeGenerators.javaFile(src)))
  } catch { case e: Exception  =>
    error("compile failed : " + sir.filePath, e)
    None
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

  private class JavaSourceObject (uri: URI, src: String) extends SimpleJavaFileObject (uri, JavaFileObject.Kind.SOURCE) {
    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = src
    def getSource: String = src
  }
}

