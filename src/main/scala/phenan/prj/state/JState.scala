package phenan.prj.state

import org.slf4j._

import scala.util._

class JState private (val searchPath: JSearchPath, val classPath: String, val destination: String) {
  def uniqueId: Int = {
    val uid = uniqueNum
    uniqueNum += 1
    uid
  }

  def successOrError [T] (x: Try[T], msg: => String, default: => T): T = x match {
    case Success(t) => t
    case Failure(e) => error(msg, e); default
  }

  def someOrError [T] (x: Option[T], msg: => String, default: => T): T = x match {
    case Some(t) => t
    case None => error(msg); default
  }

  def error (msg: => String): Unit = {
    logger.error(msg)
    nErrors += 1
  }

  def error (msg: => String, e: Throwable): Unit = {
    logger.error(msg, e)
    nErrors += 1
  }

  def errorAndReturn [T] (msg: => String, default: T): T = {
    error(msg)
    default
  }

  def errorAndReturn [T] (msg: => String, e: Throwable, default: T): T = {
    error(msg, e)
    default
  }

  def warn (msg: => String): Unit = {
    logger.warn(msg)
    nWarns += 1
  }

  def info (msg: => String): Unit = {
    logger.info(msg)
  }

  def clean(): Unit = {
    searchPath.close()
  }

  def warns: Int = nWarns
  def errors: Int = nErrors

  private var nErrors: Int = 0
  private var nWarns: Int = 0
  private var uniqueNum: Int = 0
  private lazy val logger = LoggerFactory.getLogger("pjc")
}

object JState {
  def apply (javaHome: String, classPath: String, destination: String, sourcePath: Option[String]): Try[JState] = for {
    searchPath <- JSearchPath.configure(javaHome, classPath, sourcePath.getOrElse(classPath))
  } yield new JState(searchPath, classPath, destination)
}