package phenan.prj.internal

import java.io._

import scala.util.Try

trait ByteParsers {
  val u1  = ByteParser(_.u1)
  val u2  = ByteParser(_.u2)
  val s1  = ByteParser(_.s1)
  val s2  = ByteParser(_.s2)
  val s4  = ByteParser(_.s4)
  val s8  = ByteParser(_.s8)
  val f4  = ByteParser(_.f4)
  val f8  = ByteParser(_.f8)
  val utf = ByteParser(_.utf)

  def ret [T] (v: => T) = ByteParser.pure(Try(v))

  def pure [T] (v: => Try[T]) = ByteParser.pure(v)

  def failure (e: Exception) = ByteParser.failure(e)

  def bytes (n: Int) = ByteParser(_.bytes(n))
  def padding (n: Int) = ByteParser(_.padding(n))

  def list [T] (p: ByteParser[T]): ByteParser[List[T]] = u2 >>= p.rep

  def ref [T] (p: ByteParser[T]): ByteParser[T] = ByteParser.ref(p)


  def parseFile[T] (file: String)(parser: ByteParser[T]): Try[T] = ByteReader.openFile(file)(parser)

  def parseString[T] (string: String)(parser: ByteParser[T]): Try[T] = ByteReader.openString(string)(parser)

  def parse[T] (file: File)(parser: ByteParser[T]): Try[T] = ByteReader.open(file)(parser)

  def parse[T] (bytes: Array[Byte])(parser: ByteParser[T]): Try[T] = ByteReader.open(bytes)(parser)

  def parse[T] (stream: InputStream)(parser: ByteParser[T]): Try[T] = ByteReader.open(stream)(parser)
}
