package phenan.prj.generator

import phenan.util._

import scala.language.implicitConversions

trait Generators {
  def spacingBeforeWord: List[Char]
  def spacingAfterWord: List[Char]

  class Generator[-T] private[Generators] (private[Generators] val g: (StringBuilder, Int, Boolean, T) => Boolean) {
    def ~ [U] (that: => Generator[U]): Generator[(T, U)] = new Generator[(T, U)]((buf, indent, spacing, tu) => that.g(buf, indent, g(buf, indent, spacing, tu._1), tu._2))
    def <~ [U] (that: => Generator[U])(implicit e: EmptyInput[U]): Generator[T] = new Generator[T]((buf, indent, spacing, t) => that.g(buf, indent, g(buf, indent, spacing, t), e.v))
    def ~> [U, TT <: T] (that: => Generator[U])(implicit e: EmptyInput[TT]): Generator[U] = new Generator[U]((buf, indent, spacing, u) => that.g(buf, indent, g(buf, indent, spacing, e.v), u))
    def indented: Generator[T] = new Generator[T]((buf, indent, spacing, t) => g(buf, indent + 1, spacing, t))

    def ? : Generator[Option[T]] = new Generator[Option[T]]((buf, indent, spacing, optional) => optional.fold(spacing)(g(buf, indent, spacing, _)))
    def ? [U] (default: Generator[U])(implicit e: EmptyInput[U]) : Generator[Option[T]] = new Generator[Option[T]]((buf, indent, spacing, optional) => optional.fold(default.g(buf, indent, spacing, e.v))(g(buf, indent, spacing, _)))

    def * : Generator[List[T]] = new Generator[List[T]]((buf, indent, spacing, list) => list.foldLeft(spacing) { (s, t) => g(buf, indent, s, t) })
    def * [U] (sep: Generator[U])(implicit e: EmptyInput[U]): Generator[List[T]] = new Generator[List[T]]((buf, indent, spacing, list) => list match {
      case head :: tail => tail.foldLeft(g(buf, indent, spacing, head)) { (s, t) => g(buf, indent, sep.g(buf, indent, s, e.v), t) }
      case Nil          => spacing
    })

    def *? [U] (start: Generator[U], sep: Generator[U], end: Generator[U])(implicit e: EmptyInput[U]): Generator[List[T]] = new Generator[List[T]]((buf, indent, spacing, list) => list match {
      case head :: tail => end.g(buf, indent, tail.foldLeft(g(buf, indent, start.g(buf, indent, spacing, e.v), head)) { (s, t) => g(buf, indent, sep.g(buf, indent, s, e.v), t) }, e.v)
      case Nil          => spacing
    })

    def apply(t: T): String = {
      val buf = new StringBuilder
      g(buf, 0, false, t)
      buf.toString()
    }
  }

  implicit class GeneratorCombinator [T] (generator: => Generator[T]) {
    def ^^ [U] (f: U => T): Generator[U] = new Generator[U]((buf, indent, spacing, u) => generator.g(buf, indent, spacing, f(u)))
  }

  implicit class UnionGenerator [T <: Union] (generator: => Generator[T]) {
    def :|: [U] (that: Generator[U]): Generator[U :|: T] = new Generator[U :|: T]((buf, indent, spacing, union) => union match {
      case -:|: (u) => that.g(buf, indent, spacing, u)
      case :|:- (t) => generator.g(buf, indent, spacing, t)
    })
  }

  implicit def unit (c: Char): Generator[Unit] = new Generator[Unit] ((buf, _, spacing, _) => {
    if (spacing && spacingBeforeWord.contains(c)) buf.append(' ')
    buf.append(c)
    spacingAfterWord.contains(c)
  })

  implicit def unit (s: String): Generator[Unit] = new Generator[Unit] ((buf, _, spacing, _) => appendString(buf, spacing, s))

  def newLine: Generator[Unit] = new Generator[Unit]((buf, indent, _, _) => {
    buf.append('\n').append(indentation(indent))
    false
  })

  def indent [T] (g: Generator[T]): Generator[T] = (newLine ~> g).indented <~ newLine

  def string: Generator[String] = elem(a => a)

  def ␣ : Generator[Unit] = new Generator[Unit]((_, _, _, _) => true)

  def nil : Generator[UNil] = new Generator[UNil]((_, _, _, _) => false)

  def elem [T] (f: T => String): Generator[T] = new Generator[T] ((buf, _, spacing, t) => appendString(buf, spacing, f(t)))

  def lexical [T] (f: T => String): Generator[T] = new Generator[T] ((buf, _, _, t) => {
    buf.append(f(t))
    false
  })

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


