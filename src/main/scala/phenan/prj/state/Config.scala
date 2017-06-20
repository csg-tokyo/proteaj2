package phenan.prj.state

import java.io.File
import java.util.jar.JarFile

/**
  * Created by ichikawa on 2017/06/14.
  */
case class Config
(
  javaHome: File = new File(System.getProperty("java.home")),
  destination: File = new File("."),
  classPath: Stream[SearchPathEntry] = Stream(DirectoryPath(new File("."))),
  sourcePath: Stream[SearchPathEntry] = Stream.empty,
  displayJavaSources: Boolean = false,
  files: List[File] = Nil
)
{
  def getClassPathString: String = (classPath ++ systemClassPath).map {
    case DirectoryPath(dir) => dir.getPath
    case JarPath(jar)       => jar.getName
  }.mkString(File.pathSeparator)

  def findClassFile (name: String): Option[FoundResource] = {
    classPath.flatMap(_.find(name + ".class")).headOption orElse systemClassPath.flatMap(_.find(name + ".class")).headOption
  }

  def findSourceFile (name: String): Option[FoundResource] = {
    sourcePath.flatMap(_.find(name + ".pj")).headOption orElse sourcePath.flatMap(_.find(name + ".java")).headOption
  }

  lazy val systemClassPath: Stream[SearchPathEntry] = {
    val javaRT = for {
      lib <- javaHome.listFiles(file => file.getName == "lib").headOption
      jar <- lib.listFiles(file => file.getName == "rt.jar").headOption
    } yield JarPath(new JarFile(jar))

    val path = Thread.currentThread().getContextClassLoader.getResource("proteaj/lang/PredefOperators.class").getPath.stripSuffix("proteaj/lang/PredefOperators.class")
    val proteaJRT = DirectoryPath(new File(path))

    proteaJRT +: javaRT.toStream
  }
}

object Config {
  def configure (args: Array[String]): Option[Config] = {
    parser.parse(args, Config())
  }

  def showUsage (): Unit = parser.showUsage()

  private implicit val searchPathReader: scopt.Read[Stream[SearchPathEntry]] = scopt.Read.reads { str =>
    str.split(File.pathSeparator).map(scopt.Read.fileRead.reads).map {
      case dir if dir.isDirectory => DirectoryPath(dir)
      case jar if jar.getName.endsWith(".jar") || jar.getName.endsWith(".zip") => JarPath(new JarFile(jar))
      case els => throw new IllegalArgumentException("'" + els + "' is not a search path entry.")
    } (collection.breakOut)
  }

  private val parser = new scopt.OptionParser[Config] ("<sbt run>") {
    head("ProteaJ Compiler")

    opt[File]("javahome").
      valueName("<directory>").
      validate(file => Either.cond(file.isDirectory, (), s"${file.getPath} is not directory")).
      action((file, config) => config.copy(javaHome = file)).
      text("specify non-default java home directory")

    opt[File]('d', "destination").
      valueName("<directory>").
      validate(file => Either.cond(file.isDirectory, (), s"${file.getPath} is not directory")).
      action((file, config) => config.copy(destination = file)).
      text("specify where to place generated class files")

    opt[Stream[SearchPathEntry]]("classpath").abbr("cp").
      valueName("<path>").
      action((path, config) => config.copy(classPath = path)).
      text("specify where to find user class files")

    opt[Stream[SearchPathEntry]]("sourcepath").
      valueName("<path>").
      action((path, config) => config.copy(sourcePath = path)).
      text("specify where to find user source files")

    opt[Unit]("printsources").
      action((_, config) => config.copy(displayJavaSources = true)).
      text("print generated Java source programs")

    help("help").abbr("h").text("prints this usage text")

    arg[File]("<file>...").unbounded().optional().
      validate(file => Either.cond(file.isFile && file.canRead, (), s"${file.getPath} is not readable file")).
      action((file, config) => config.copy(files = config.files :+ file))

    override def showUsageOnError = true
  }
}


