package phenan.prj

trait JirConfig {
  def javaHome: String
  def classPath: String
}

object JirConfig {
  implicit val default: JirConfig = new JirConfig {
    override def javaHome: String = System.getProperty("java.home")
    override def classPath: String = "."
  }
}

class JirConfigBuilder () {
  self =>

  private val d = JirConfig.default

  var javaHome: String = d.javaHome
  var classPath: String = d.classPath

  def make: JirConfig = new JirConfig {
    override def javaHome: String = self.javaHome
    override def classPath: String = self.classPath
  }
}
