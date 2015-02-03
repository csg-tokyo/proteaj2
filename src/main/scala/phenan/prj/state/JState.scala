package phenan.prj.state

class JState private[state] (val searchPath: JSearchPath) {
  def clean(): Unit = {
    searchPath.close()
  }
}
