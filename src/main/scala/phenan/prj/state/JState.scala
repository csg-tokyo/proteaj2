package phenan.prj.state

import org.slf4j._

class JState private[state] (val searchPath: JSearchPath) {
  def error (msg: => String): Unit = {
    logger.error(msg)
    nErrors += 1
  }

  def error (msg: => String, e: Throwable): Unit = {
    logger.error(msg, e)
    nErrors += 1
  }

  def warn (msg: => String): Unit = {
    logger.warn(msg)
    nWarns += 1
  }

  def clean(): Unit = {
    searchPath.close()
  }

  def warns: Int = nWarns
  def errors: Int = nErrors

  private var nErrors: Int = 0
  private var nWarns: Int = 0
  private lazy val logger = LoggerFactory.getLogger("pjc")
}
