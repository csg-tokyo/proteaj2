package phenan.prj.state

import java.io._
import java.util.jar._

/**
  * Created by ichikawa on 2017/06/15.
  */
trait FoundResource {
  def name: String
  def openInputStream: InputStream
  def openFileReader: Reader
}

case class FoundFile (file: File) extends FoundResource {
  override def name: String = file.getPath
  override def openInputStream: InputStream = new BufferedInputStream(new FileInputStream(file))
  override def openFileReader: Reader = new BufferedReader(new FileReader(file))
}

case class FoundJarEntry (entry: JarEntry, jar: JarFile) extends FoundResource {
  override def name: String = entry.getName
  override def openInputStream: InputStream = new BufferedInputStream(jar.getInputStream(entry))
  override def openFileReader: Reader = new BufferedReader(new InputStreamReader(jar.getInputStream(entry)))
}

trait SearchPathEntry {
  def find (name: String): Option[FoundResource]
  def close (): Unit
}

case class DirectoryPath (directory: File) extends SearchPathEntry {
  require(directory.isDirectory)

  def find (name: String): Option[FoundFile] = {
    if (name.isEmpty) None
    else find(name.split(File.separatorChar).toList, directory)
  }

  def close (): Unit = ()

  private def find (names: List[String], dir: File): Option[FoundFile] = names match {
    case name :: rest => dir.listFiles(_.getName == name) match {
      case Array(f) => find(rest, f)
      case _        => None
    }
    case _ => Some(FoundFile(dir))
  }
}

case class JarPath (jar: JarFile) extends SearchPathEntry {
  def find (name: String): Option[FoundJarEntry] = Option(jar.getJarEntry(name)).map(FoundJarEntry(_, jar))
  def close (): Unit = jar.close()
}
