package phenan.prj.combinator

import scala.util._

trait ByteParser[+T] {
  def perform (reader: ByteReader): Try[T]

  def map [U] (f: T => U): ByteParser[U]
  def flatMap [U] (f: T => ByteParser[U]): ByteParser[U]

  def filter (f: T => Boolean): ByteParser[T]
  def withFilter (f: T => Boolean): ByteParserWithFilter[T]

  def rep (n: Int): ByteParser[List[T]]

  def ^^ [U] (f: T => U): ByteParser[U] = map(f)
  def >>= [U] (f: T => ByteParser[U]): ByteParser[U] = flatMap(f)
}

trait ByteParserWithFilter [+T] {
  def map [U] (g: T => U): ByteParser[U]
  def flatMap [U] (g: T => ByteParser[U]): ByteParser[U]
  def withFilter (g: T => Boolean): ByteParserWithFilter[T]
}

object ByteParser {
  def pure [T] (f: => Try[T]): ByteParser[T] = new SimpleParser[T](_ => f)

  def apply [T] (f: ByteReader => T): ByteParser[T] = new SimpleParser[T](r => Try(f(r)))

  def ref [T] (p: => ByteParser[T]): ByteParser[T] = new ParserRef[T](p)

  def failure (e: Exception): ByteParser[Nothing] = new SimpleParser[Nothing](_ => scala.util.Failure(e))

  private class SimpleParser [T] (f: ByteReader => Try[T]) extends ByteParser[T] {
    override def perform(reader: ByteReader): Try[T] = f(reader)
    override def map [U](g: T => U): ByteParser[U] = new SimpleParser[U](r => f(r).map(g))
    override def flatMap[U](g: T => ByteParser[U]): ByteParser[U] = new SimpleParser[U](r => f(r).flatMap(t => g(t).perform(r)))
    override def filter(g: T => Boolean): ByteParser[T] = new SimpleParser[T](r => f(r).filter(g))
    override def withFilter(g: T => Boolean): ByteParserWithFilter[T] = new SimpleParserWithFilter[T](f, g)
    override def rep(n: Int): ByteParser[List[T]] = rep(n, pure(scala.util.Success(Nil)))
    private def rep (n: Int, parser: ByteParser[List[T]]): ByteParser[List[T]] = {
      if (n > 0) rep(n - 1, new SimpleParser[List[T]] ({ r: ByteReader =>
        for (v <- f(r); vs <- parser.perform(r)) yield v :: vs
      }))
      else parser
    }
  }

  private class SimpleParserWithFilter [T] (f: ByteReader => Try[T], g: T => Boolean) extends ByteParserWithFilter[T] {
    override def map[U](h: T => U): ByteParser[U] = new SimpleParser[U](r => f(r).withFilter(g).map(h))
    override def flatMap[U](h: T => ByteParser[U]): ByteParser[U] = new SimpleParser[U](r => f(r).withFilter(g).flatMap(t => h(t).perform(r)))
    override def withFilter(h: T => Boolean): ByteParserWithFilter[T] = new SimpleParserWithFilter[T](f, { t => g(t) && h(t) })
  }

  private class ParserRef [T] (p: => ByteParser[T]) extends ByteParser[T] {
    lazy val parser: ByteParser[T] = p
    override def perform(reader: ByteReader): Try[T] = parser.perform(reader)
    override def map [U](g: T => U): ByteParser[U] = parser.map(g)
    override def flatMap[U](g: T => ByteParser[U]): ByteParser[U] = parser.flatMap(g)
    override def filter(g: T => Boolean): ByteParser[T] = parser.filter(g)
    override def withFilter(g: T => Boolean): ByteParserWithFilter[T] = parser.withFilter(g)
    override def rep(n: Int): ByteParser[List[T]] = parser.rep(n)
  }
}

