package phenan.jir.internal

import phenan.util._

import scala.util._

import scalaz._
import Scalaz._

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

  def failure (e: Exception): ByteParser[Nothing] = new SimpleParser[Nothing](_ => scala.util.Failure(e))

  private class SimpleParser [T] (f: ByteReader => Try[T]) extends ByteParser[T] {
    override def perform(reader: ByteReader): Try[T] = f(reader)
    override def map [U](g: T => U): ByteParser[U] = new SimpleParser[U](r => f(r).map(g))
    override def flatMap[U](g: T => ByteParser[U]): ByteParser[U] = new SimpleParser[U](r => f(r).flatMap(t => g(t).perform(r)))
    override def filter(g: T => Boolean): ByteParser[T] = new SimpleParser[T](r => f(r).filter(g))
    override def withFilter(g: T => Boolean): ByteParserWithFilter[T] = new SimpleParserWithFilter[T](f, g)
    override def rep(n: Int): ByteParser[List[T]] = new SimpleParser[List[T]](r => List.range(0, n).traverse(_ => perform(r)))
  }

  private class SimpleParserWithFilter [T] (f: ByteReader => Try[T], g: T => Boolean) extends ByteParserWithFilter[T] {
    override def map[U](h: T => U): ByteParser[U] = new SimpleParser[U](r => f(r).withFilter(g).map(h))
    override def flatMap[U](h: T => ByteParser[U]): ByteParser[U] = new SimpleParser[U](r => f(r).withFilter(g).flatMap(t => h(t).perform(r)))
    override def withFilter(h: T => Boolean): ByteParserWithFilter[T] = new SimpleParserWithFilter[T](f, { t => g(t) && h(t) })
  }
}

