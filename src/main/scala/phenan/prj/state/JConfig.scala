package phenan.prj.state

import scala.util.Try

class JConfig {
  var javaHome: String = System.getProperty("java.home")
  var classPath: String = "."

  def sourcePath_= (path: String): Unit = {
    sourcePathField = Some(path)
  }

  def sourcePath = sourcePathField

  private var sourcePathField: Option[String] = None

  def configure: Try[JState] = for {
    searchPath <- JSearchPath.configure(javaHome, classPath, sourcePathField.getOrElse(classPath))
  } yield new JState(searchPath)
}

object JConfig {
  def apply () = new JConfig
}
