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
  def name: String
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
  override def name: String = file.getName
  override def in: Reader = new BufferedReader(new FileReader(file))
  override def lastModified: Long = file.lastModified()
}

private[state] class FoundSourceFileFromJarPath (entry: JarEntry, jar: JarFile) extends FoundSourceFile {
  override def name: String = entry.getName
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
  import phenan.util.TryUtil._
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
