package phenan.prj.config

trait JConfig {
  def classPath: JSearchPath
  def sourcePath: JSearchPath
}

object JConfig {
  def default: JConfig = new JConfigBuilder().make
}

class JConfigBuilder () {
  self =>

  var javaHome: String = System.getProperty("java.home")
  var classPath: String = "."

  def sourcePath_= (path: String): Unit = {
    sourcePathField = Some(path)
  }

  def sourcePath = sourcePathField

  private var sourcePathField: Option[String] = None

  def make: JConfig = new JConfig {
    val classPath = JSearchPath.classPath(self.javaHome, self.classPath).get
    val sourcePath = JSearchPath.sourcePath(self.sourcePathField.getOrElse(self.classPath)).get
  }
}
