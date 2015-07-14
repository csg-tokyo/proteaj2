package phenan.prj.generator

import phenan.prj.exception.InvalidASTException

import scala.language.implicitConversions
import scala.util._

trait Generators {
  def spacingBeforeWord: List[Char]
  def spacingAfterWord: List[Char]

  type | [+A, +B] = Either[A, B]

  class Generator[-T] private[Generators] (private val g: (StringBuilder, Int, Boolean, T) => Boolean) {
    def ~ [U] (that: => Generator[U]): Generator[(T, U)] = new Generator[(T, U)]((buf, indent, spacing, tu) => that.g(buf, indent, g(buf, indent, spacing, tu._1), tu._2))
    def <~ [U] (that: => Generator[U])(implicit e: EmptyInput[U]): Generator[T] = new Generator[T]((buf, indent, spacing, t) => that.g(buf, indent, g(buf, indent, spacing, t), e.v))
    def ~> [U, TT <: T] (that: => Generator[U])(implicit e: EmptyInput[TT]): Generator[U] = new Generator[U]((buf, indent, spacing, u) => that.g(buf, indent, g(buf, indent, spacing, e.v), u))
    def ^^ [U] (f: U => T): Generator[U] = new Generator[U]((buf, indent, spacing, u) => g(buf, indent, spacing, f(u)))
    def indented: Generator[T] = new Generator[T]((buf, indent, spacing, t) => g(buf, indent + 1, spacing, t))

    def ^? [U] (f: PartialFunction[U, T]): Generator[U] = ^^ { u: U => f.applyOrElse(u, { v: U => throw InvalidASTException("compiler cannot generate Java code for " + v) }) }

    def ? : Generator[Option[T]] = new Generator[Option[T]]((buf, indent, spacing, optional) => optional.fold(spacing)(g(buf, indent, spacing, _)))
    def * : Generator[List[T]] = new Generator[List[T]]((buf, indent, spacing, list) => list.foldLeft(spacing) { (s, t) => g(buf, indent, s, t) })
    def * [U] (sep: Generator[U])(implicit e: EmptyInput[U]): Generator[List[T]] = new Generator[List[T]]((buf, indent, spacing, list) => list match {
      case head :: tail => tail.foldLeft(g(buf, indent, spacing, head)) { (s, t) => g(buf, indent, sep.g(buf, indent, s, e.v), t) }
      case Nil          => spacing
    })

    def *? [U] (start: Generator[U], sep: Generator[U], end: Generator[U])(implicit e: EmptyInput[U]): Generator[List[T]] = new Generator[List[T]]((buf, indent, spacing, list) => list match {
      case head :: tail => end.g(buf, indent, tail.foldLeft(g(buf, indent, start.g(buf, indent, spacing, e.v), head)) { (s, t) => g(buf, indent, sep.g(buf, indent, s, e.v), t) }, e.v)
      case Nil          => spacing
    })

    def | [U] (that: => Generator[U]): Generator[T | U] = new Generator[T | U]((buf, indent, spacing, either) => either.fold(g(buf, indent, spacing, _), that.g(buf, indent, spacing, _)))

    def apply(t: T): Try[String] = try {
      val buf = new StringBuilder
      g(buf, 0, false, t)
      Success(buf.toString())
    } catch {
      case e: InvalidASTException => Failure(e)
    }
  }

  implicit def unit (c: Char): Generator[Unit] = new Generator[Unit] ((buf, _, spacing, _) => {
    if (spacing && spacingBeforeWord.contains(c)) buf.append(' ')
    buf.append(c)
    spacingAfterWord.contains(c)
  })

  implicit def unit (s: String): Generator[Unit] = new Generator[Unit] ((buf, _, spacing, _) => appendString(buf, spacing, s))

  implicit def eitherUtil [T] (v: T): EitherUtil[T] = new EitherUtil[T](v)

  def newLine: Generator[Unit] = new Generator[Unit]((buf, indent, _, _) => {
    buf.append('\n').append(indentation(indent))
    false
  })

  def string: Generator[String] = elem(a => a)

  def ␣ : Generator[Unit] = new Generator[Unit]((_, _, _, _) => true)

  def elem [T] (f: T => String): Generator[T] = new Generator[T] ((buf, _, spacing, t) => appendString(buf, spacing, f(t)))

  private def indentation (depth: Int): String = (0 until depth).map(_ => "  ").mkString

  private def appendString (buf: StringBuilder, spacing: Boolean, s: String): Boolean = {
    if (s.isEmpty) spacing
    else {
      if (spacing) buf.append(' ')
      buf.append(s)
      true
    }
  }
}

class EmptyInput[T] (val v: T) extends AnyVal

object EmptyInput {
  implicit val unit: EmptyInput[Unit] = new EmptyInput(())
}

class EitherUtil[T] (val v: T) extends AnyVal {
  type | [+A, +B] = Either[A, B]

  def l : T | Nothing = Left(v)
  def r : Nothing | T = Right(v)
}
