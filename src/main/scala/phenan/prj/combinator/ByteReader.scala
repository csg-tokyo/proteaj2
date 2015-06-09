package phenan.prj.combinator

import java.io._

import scala.util.Try

class ByteReader private (in: DataInputStream) {

  def u1: Int    = read(1, in.readUnsignedByte())
  def u2: Int    = read(2, in.readUnsignedShort())
  def s1: Int    = read(1, in.readByte())
  def s2: Int    = read(2, in.readShort())
  def s4: Int    = read(4, in.readInt())
  def s8: Long   = read(8, in.readLong())
  def f4: Float  = read(4, in.readFloat())
  def f8: Double = read(8, in.readDouble())

  def bytes(size: Int): Array[Byte] = {
    val bs = new Array[Byte](size)
    read(size, in.read(bs))
    bs
  }

  def utf: String = new String(bytes(u2), "UTF-8")

  def padding(n: Int): Array[Byte] = {
    val rem = (pos % n).toInt
    if (rem == 0) Array.empty
    else bytes(n - rem)
  }

  def position = pos

  private def close() { in.close() }

  private def read[T] (size: Int, reader: => T): T = {
    pos += size
    reader
  }

  private var pos: Long = 0
}

object ByteReader {
  def openFile[T] (file: String)(p: ByteParser[T]): Try[T] = open(new File(file))(p)

  def openString[T] (string: String)(p: ByteParser[T]): Try[T] = open(string.getBytes("UTF-8"))(p)

  def open[T] (file: File)(p: ByteParser[T]): Try[T] = {
    Try(new FileInputStream(file)).flatMap(in => open(in)(p))
  }

  def open[T] (bytes: Array[Byte])(p: ByteParser[T]): Try[T] = {
    Try(new ByteArrayInputStream(bytes)).flatMap(in => open(in)(p))
  }

  def open[T] (in: InputStream)(p: ByteParser[T]): Try[T] = for {
    reader <- Try(new ByteReader(new DataInputStream(new BufferedInputStream(in))))
    ret    <- p.perform(reader)
    _      <- Try(reader.close())
  } yield ret
}
