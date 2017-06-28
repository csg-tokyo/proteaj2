package phenan.prj.combinator

import scala.reflect.ClassTag
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.input.{Positional, Reader}

trait TwoLevelParsers {
  self =>

  type Elem

  def delimiter: LParser[Any]

  type Input = Reader[Elem]
  type ParseResult[+T] = Impl.ParseResult[T]
  type ~ [+A, +B] = Impl.~[A, B]

  val ~ : Impl.~.type = Impl.~
  val ParseSuccess = Impl.Success
  val ParseFailure = Impl.NoSuccess

  def accept [T] (kind: String, f: PartialFunction[Elem, T]): LParser[T] = new LParser(Impl.accept(kind, f))
  def elem (e: Elem): LParser[Elem] = new LParser(Impl.elem(e))
  def elem (kind: String, f: Elem => Boolean): LParser[Elem] = new LParser(Impl.elem(kind, f))
  def elem [E <: Elem] (implicit Tag: ClassTag[E]): LParser[E] = new LParser(Impl.accept(Tag.toString(), { case Tag(x) => x }))

  def positioned [T <: Positional] (parser: HParser[T]): HParser[T] = new HParser(Impl.positioned(parser.parser))

  object HParser {
    def success [T] (t: => T): HParser[T] = new HParser(Impl.success(t))
    def failure (msg: => String): HParser[Nothing] = new HParser(Impl.failure(msg))
  }

  object LParser {
    def success [T] (t: => T): LParser[T] = new LParser(Impl.success(t))
    def failure (msg: => String): LParser[Nothing] = new LParser(Impl.failure(msg))
  }

  class HParser[+T] (getParser: => Impl.PackratParser[T]) {
    private[TwoLevelParsers] lazy val parser = getParser

    def apply (in: Input): ParseResult[T] = (delimiter.parser ~> parser <~ delimiter.parser).apply(new Impl.PackratReader[Elem](in))

    def ~ [U] (that: => HParser[U]): HParser[T ~ U] = new HParser((this.parser <~ delimiter.parser) ~ that.parser)
    def ~> [U] (that: => HParser[U]): HParser[U] = new HParser(this.parser ~> delimiter.parser ~> that.parser)
    def <~ [U] (that: => HParser[U]): HParser[T] = new HParser(this.parser <~ delimiter.parser <~ that.parser)

    def | [U >: T] (that: => HParser[U]): HParser[U] = new HParser(this.parser | that.parser)
    def ||| [U >: T] (that: => HParser[U]): HParser[U] = new HParser(this.parser ||| that.parser)

    def ? : HParser[Option[T]] = new HParser(parser.?)
    def & : HParser[T] = new HParser(Impl.guard(parser))
    def ! : HParser[Unit] = new HParser(Impl.not(parser))
    def * : HParser[List[T]] = new HParser(Impl.repsep(parser, delimiter.parser))
    def + : HParser[List[T]] = new HParser(Impl.rep1sep(parser, delimiter.parser))
    def * [U] (sep: => HParser[U]): HParser[List[T]] = new HParser(Impl.repsep(parser, delimiter.parser ~> sep.parser ~> delimiter.parser))
    def + [U] (sep: => HParser[U]): HParser[List[T]] = new HParser(Impl.rep1sep(parser, delimiter.parser ~> sep.parser ~> delimiter.parser))

    def map [R] (f: T => R): HParser[R] = new HParser(parser.map(f))
    def mapOption [R] (f: T => Option[R]): HParser[R] = new HParser(parser ^^ f ^? { case Some(r) => r })
    def flatMap [R] (f: T => HParser[R]): HParser[R] = new HParser((parser <~ delimiter.parser).flatMap(f(_).parser))

    def ^? [R] (f: PartialFunction[T, R]): HParser[R] = new HParser(parser ^? f)
    def ^^ [R] (f: T => R): HParser[R] = map(f)
    def ^^? [R] (f: T => Option[R]): HParser[R] = mapOption(f)
    def ^^^ [R] (f: => R): HParser[R] = new HParser(parser ^^^ f)
    def >> [R] (f: T => HParser[R]): HParser[R] = flatMap(f)

    def log (s: String): HParser[T] = new HParser(Impl.log(parser)(s))
  }

  class LParser[+T] (getParser: => Impl.PackratParser[T]) {
    private[TwoLevelParsers] lazy val parser = getParser
    lazy val ^ : HParser[T] = new HParser(parser)

    def ~ [U] (that: => LParser[U]): LParser[T ~ U] = new LParser(this.parser ~ that.parser)
    def ~> [U] (that: => LParser[U]): LParser[U] = new LParser(this.parser ~> that.parser)
    def <~ [U] (that: => LParser[U]): LParser[T] = new LParser(this.parser <~ that.parser)

    def | [U >: T] (that: => LParser[U]): LParser[U] = new LParser(this.parser | that.parser)
    def ||| [U >: T] (that: => LParser[U]): LParser[U] = new LParser(this.parser ||| that.parser)

    def ? : LParser[Option[T]] = new LParser(parser.?)
    def & : LParser[T] = new LParser(Impl.guard(parser))
    def ! : LParser[Unit] = new LParser(Impl.not(parser))
    def * : LParser[List[T]] = new LParser(parser.*)
    def + : LParser[List[T]] = new LParser(parser.+)
    def * [U] (sep: => LParser[U]): LParser[List[T]] = new LParser(Impl.repsep(parser, sep.parser))
    def + [U] (sep: => LParser[U]): LParser[List[T]] = new LParser(Impl.rep1sep(parser, sep.parser))

    def map [R] (f: T => R): LParser[R] = new LParser(parser.map(f))
    def mapOption [R] (f: T => Option[R]): LParser[R] = new LParser(parser ^^ f ^? { case Some(r) => r })
    def flatMap [R] (f: T => LParser[R]): LParser[R] = new LParser(parser.flatMap(f(_).parser))

    def ^? [R] (f: PartialFunction[T, R]): LParser[R] = new LParser(parser ^? f)
    def ^^ [R] (f: T => R): LParser[R] = map(f)
    def ^^? [R] (f: T => Option[R]): LParser[R] = mapOption(f)
    def ^^^ [R] (f: => R): LParser[R] = new LParser(parser ^^^ f)
    def >> [R] (f: T => LParser[R]): LParser[R] = flatMap(f)

    def log (s: String): LParser[T] = new LParser(Impl.log(parser)(s))
  }

  private[combinator] object Impl extends PackratParsers {
    override type Elem = self.Elem
  }
}