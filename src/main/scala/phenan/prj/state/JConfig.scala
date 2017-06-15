package phenan.prj.state

import java.util

import org.kohsuke.args4j.{Option => Opt, _}

import scala.collection.JavaConversions._
import scala.util.{Failure, Success, Try}
import scala.util.control.Exception._

class JConfig  {
  @Opt(name = "-javahome", metaVar = "<directory>", usage = "specify non-default java home directory")
  var javaHome: String = System.getProperty("java.home")

  @Opt(name = "-cp", aliases = Array("-classpath"), metaVar = "<path>", usage = "specify where to find user class files")
  var classPath: String = "."

  @Opt(name = "-d", metaVar = "<directory>", usage = "specify where to place generated class files")
  var destination = "."

  @Opt(name = "-sourcepath", metaVar = "<path>", usage = "specify where to find user source files")
  private var sourcePathField: String = null

  @Opt(name = "-help", usage = "usage information")
  var helpFlag: Boolean = false

  @Argument
  private var args: java.util.List[String] = new util.ArrayList[String]()

  def sourcePath_= (path: String): Unit = {
    sourcePathField = path
  }

  def sourcePath = Option(sourcePathField)

  def configure: Try[JState] = JState(javaHome, classPath, destination, sourcePath)

  def getArgs: List[String] = args.toList
}

object JConfig {
  def apply () = new JConfig

  def parseCommandLineArgs (args: Array[String]): Option[(JState, List[String])] = {
    val config = new JConfig
    val parser = new CmdLineParser(config, ParserProperties.defaults().withUsageWidth(100))

    allCatch either parser.parseArgument(args.toList) match {
      case Right(_) if config.helpFlag =>
        parser.printUsage(System.out)
        None
      case Right(_) if config.getArgs.isEmpty => printSimpleUsage(parser)
      case Right(_) => configure(config, parser)
      case Left(e)  => printError(parser, e)
    }
  }

  private def proteaJLibPath: String = {
    Thread.currentThread().getContextClassLoader.getResource("proteaj/lang/PredefOperators.class").getPath.stripSuffix("proteaj/lang/PredefOperators.class")
  }

  private def configure (config: JConfig, parser: CmdLineParser): Option[(JState, List[String])] = {
    config.classPath = config.classPath  + ':' + proteaJLibPath
    config.configure match {
      case Success(state) => Some((state, config.getArgs))
      case Failure(e)     => printError(parser, e)

    }
  }

  private def printSimpleUsage (parser: CmdLineParser) = {
    parser.printSingleLineUsage(System.out)
    println()
    None
  }

  private def printError (parser: CmdLineParser, e: Throwable) = {
    System.out.println(e.getMessage)
    parser.printSingleLineUsage(System.out)
    println()
    None
  }
}
