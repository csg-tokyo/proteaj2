package phenan.prj

import phenan.prj.state.{Config, FoundFile, FoundResource}
import java.io.File

import org.slf4j.LoggerFactory

import scala.util._

/**
  * Created by ichikawa on 2017/06/15.
  */
trait Application {
  def config: Config

  def findClassFile (name: String): Option[FoundResource] = config.findClassFile(name)
  def findSourceFile (name: String): Option[FoundResource] = config.findSourceFile(name)

  def inputFiles: List[File] = config.files

  def generateUniqueId: Int = {
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
    config.classPath.foreach(_.close())
    config.sourcePath.foreach(_.close())
  }

  def warns: Int = nWarns
  def errors: Int = nErrors

  private var nErrors: Int = 0
  private var nWarns: Int = 0
  private var uniqueNum: Int = 0
  private lazy val logger = LoggerFactory.getLogger("pjc")
}
