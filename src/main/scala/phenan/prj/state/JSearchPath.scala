package phenan.prj.state

import java.io._
import java.util.jar._

import scala.util._

class JSearchPath private (classPath: Stream[SearchPathEntry], sourcePath: Stream[SearchPathEntry]) {
  def find (name: String): Option[FoundFile] = {
    val sourceFile = sourcePath.flatMap(path => path.findSourceFile(name + ".pj") orElse path.findSourceFile(name + ".java")).headOption
    val classFile = classPath.flatMap(path => path.findClassFile(name + ".class")).headOption

    if (sourceFile.isEmpty) classFile
    else if (classFile.isEmpty) sourceFile
    else if (sourceFile.get.lastModified < classFile.get.lastModified) classFile
    else sourceFile
  }
  
  private[state] def close(): Unit = {
    classPath.foreach(_.close())
    sourcePath.foreach(_.close())
  }
}

sealed trait FoundFile {
  def lastModified: Long
}

sealed trait FoundClassFile extends FoundFile {
  def in: InputStream
}

sealed trait FoundSourceFile extends FoundFile {
  def in: Reader
}

private[state] class FoundClassFileFromDirPath (file: File) extends FoundClassFile {
  override def in: InputStream = new BufferedInputStream(new FileInputStream(file))
  override def lastModified: Long = file.lastModified()
}

private[state] class FoundClassFileFromJarPath (entry: JarEntry, jar: JarFile) extends FoundClassFile {
  override def in: InputStream = new BufferedInputStream(jar.getInputStream(entry))
  override def lastModified: Long = entry.getLastModifiedTime.toMillis
}

private[state] class FoundSourceFileFromDirPath (file: File) extends FoundSourceFile {
  override def in: Reader = new BufferedReader(new FileReader(file))
  override def lastModified: Long = file.lastModified()
}

private[state] class FoundSourceFileFromJarPath (entry: JarEntry, jar: JarFile) extends FoundSourceFile {
  override def in: Reader = new BufferedReader(new InputStreamReader(jar.getInputStream(entry)))
  override def lastModified: Long = entry.getLastModifiedTime.toMillis
}

private[state] sealed trait SearchPathEntry {
  def findClassFile (name: String): Option[FoundClassFile]
  def findSourceFile (name: String): Option[FoundSourceFile]
  def close(): Unit
}

private[state] class DirectoryPath (dirName: String) extends SearchPathEntry {
  override def findClassFile (name: String): Option[FoundClassFile] = {
    findFile(name).map(file => new FoundClassFileFromDirPath(file))
  }

  override def findSourceFile (name: String): Option[FoundSourceFile] = {
    findFile(name).map(file => new FoundSourceFileFromDirPath(file))
  }

  override def close(): Unit = ()

  private def findFile (name: String): Option[File] = {
    val file =
      if (File.separatorChar == '/') new File(dirName + name)
      else new File(dirName + name.replace('/', File.separatorChar))
    if (file.canRead) Some(file)
    else None
  }
}

private[state] class JarFilePath (jarFile: JarFile) extends SearchPathEntry {
  override def findClassFile (name: String): Option[FoundClassFile] = {
    Option(jarFile.getJarEntry(name)).map(entry => new FoundClassFileFromJarPath(entry, jarFile))
  }

  override def findSourceFile (name: String): Option[FoundSourceFile] = {
    Option(jarFile.getJarEntry(name)).map(entry => new FoundSourceFileFromJarPath(entry, jarFile))
  }

  override def close(): Unit = jarFile.close()
}

object JSearchPath {
  private[state] def configure (javaHome: String, classPath: String, sourcePath: String): Try[JSearchPath] = for {
    sysPath <- parsePath(normalizeDirPath(javaHome) + "lib" + File.separatorChar + "rt.jar")
    usrPath <- parsePath(classPath)
    srcPath <- parsePath(sourcePath)
  } yield new JSearchPath(usrPath ++ sysPath, srcPath)

  /* helper method for getting JSearchPath entries */

  // for Stream.traverse
  import phenan.util._
  import scalaz.Scalaz._

  private def parsePath (path: String): Try[Stream[SearchPathEntry]] = {
    path.split(File.pathSeparator).toStream.traverse { file =>
      if (file.endsWith(".jar") || file.endsWith(".zip")) Try(new JarFilePath(new JarFile(file)))
      else Try(new DirectoryPath(normalizeDirPath(file)))
    }
  }

  /* utility method */

  private def normalizeDirPath (dir: String): String = {
    if (dir endsWith File.separator) dir
    else dir + File.separatorChar
  }
}

/*
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
}*/