package phenan.prj

import java.io._
import java.util.jar.JarFile

import phenan.util._

import scala.util.Try
import scalaz._
import Scalaz._

trait JClassPath {
  def find (name: String): Option[InputStream]
  def close(): Unit
}

object JClassPath {

  /* Factory methods */

  def get (implicit conf: JirConfig): Try[JClassPath] = for {
    usrPath <- parsePath(conf.classPath)
    sysPath <- systemPath(conf.javaHome)
  } yield new Instance(usrPath :+ sysPath)

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

  private class Instance (paths: Stream[Entry]) extends JClassPath {
    override def find(name: String): Option[InputStream] = paths.flatMap(_.find(name)).headOption
    override def close(): Unit = paths.foreach(_.close())
  }

  /* JClassPath entries */

  private sealed trait Entry {
    def find (name: String): Option[InputStream]
    def close(): Unit
  }

  private class DirPath private (dir: String) extends Entry {
    override def find(name: String): Option[InputStream] = {
      Option(new File(dir + replaceSeparator(name) + ".class")).filter(_.canRead).map(file => new FileInputStream(file))
    }
    override def close(): Unit = ()
  }

  private class JarPath private (jar: JarFile) extends Entry {
    override def find(name: String): Option[InputStream] = {
      Option(jar.getJarEntry(name + ".class")).map(jar.getInputStream)
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
    def jarPaths (path: String): Try[Stream[JarPath]] = new File(path).list(JarFilePattern).toStream.map(JarPath(_)).sequence
  }
}