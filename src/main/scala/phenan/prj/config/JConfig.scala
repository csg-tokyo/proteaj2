package phenan.prj.config

trait JConfig {
  def classPath: JSearchPath
}

object JConfig {
  def default: JConfig = new JConfigBuilder().make
}

class JConfigBuilder () {
  self =>

  var javaHome: String = System.getProperty("java.home")
  var classPath: String = "."

  def make: JConfig = new JConfig {
    val classPath: JSearchPath = JSearchPath.classPath(self.javaHome, self.classPath).get
  }
}
