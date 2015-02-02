package phenan.prj.config

import java.io._
import java.util.jar.JarFile

import phenan.util._

import scala.util.Try
import scalaz.Scalaz._

trait JSearchPath {
  def findClassFile (name: String): Option[InputStream]
  def findSourceFile (name: String): Option[Reader]
  
  def close(): Unit
}

object JSearchPath {

  /* Factory methods */

  private[config] def classPath (javaHome: String, classPath: String): Try[JSearchPath] = for {
    usrPath <- parsePath(classPath)
    sysPath <- systemPath(javaHome)
  } yield new Instance(usrPath :+ sysPath)

  private[config] def sourcePath (sourcePath: String): Try[JSearchPath] = parsePath(sourcePath).map(path => new Instance(path))

  /* helper method for getting JClassPath entries */

  private def parsePath (path: String): Try[Stream[Entry]] = {
    path.split(File.pathSeparator).toStream.traverse {
      case JarFilePattern(jar) => JarPath(jar)
      case StarPattern(dir) => JarDirPath(dir)
      case dir => DirPath(normalizeDirPath(dir))
    }
  }

  private def systemPath (javaHome: String): Try[Entry] = {
    JarPath(normalizeDirPath(javaHome) + "lib" + File.separatorChar + "rt.jar")
  }

  /* utility methods */

  private def replaceSeparator (path: String): String = {
    if (File.separatorChar == '/') path
    else path.replace('/', File.separatorChar)
  }

  private def normalizeDirPath (dir: String): String = {
    if (dir endsWith File.separator) dir
    else dir + File.separatorChar
  }

  private object StarPattern {
    def unapply (file: String): Option[String] = {
      if (file.endsWith("*")) Some(file.substring(0, file.lastIndexOf('*')))
      else None
    }
  }

  private object JarFilePattern extends FilenameFilter {
    def unapply (file: String): Option[String] = {
      if (file.endsWith(".jar") || file.endsWith(".zip")) Some(file)
      else None
    }
    override def accept (dir: File, file: String): Boolean = file.endsWith(".jar") || file.endsWith(".zip")
  }

  /* Implementation of JClassPath */

  private class Instance (paths: Stream[Entry]) extends JSearchPath {
    override def findClassFile(name: String): Option[InputStream] = paths.flatMap(_.find(name + ".class")).headOption
    override def findSourceFile (name: String): Option[Reader] = {
      paths.flatMap(path => path.find(name + ".pj") orElse path.find(name + ".java")).headOption.map(new InputStreamReader(_))
    }
    override def close(): Unit = paths.foreach(_.close())
  }

  /* JClassPath entries */

  private sealed trait Entry {
    def find (name: String): Option[InputStream]
    def close(): Unit
  }

  private class DirPath private (dir: String) extends Entry {
    override def find(name: String): Option[InputStream] = {
      Option(new File(dir + replaceSeparator(name))).filter(_.canRead).map(file => new FileInputStream(file))
    }
    override def close(): Unit = ()
  }

  private class JarPath private (jar: JarFile) extends Entry {
    override def find(name: String): Option[InputStream] = {
      Option(jar.getJarEntry(name)).map(jar.getInputStream)
    }
    override def close(): Unit = jar.close()
  }

  private class JarDirPath private (jars: Stream[JarPath]) extends Entry {
    override def find(name: String): Option[InputStream] = jars.flatMap(_.find(name)).headOption
    override def close(): Unit = jars.foreach(_.close())
  }

  private object DirPath {
    def apply (path: String): Try[DirPath] = Try(new DirPath(path))
  }

  private object JarPath {
    def apply (path: String): Try[JarPath] = Try(new JarPath(new JarFile(path)))
  }

  private object JarDirPath {
    def apply (path: String): Try[JarDirPath] = jarPaths(path).map(new JarDirPath(_))
    def jarPaths (path: String): Try[Stream[JarPath]] = new File(path).list(JarFilePattern).toStream.traverse(JarPath(_))
  }
}