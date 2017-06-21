package phenan.prj.body

import phenan.prj.exception.ParseException
import phenan.prj.ir._

import scala.reflect.ClassTag
import scala.util._
import scala.util.matching.Regex
import scala.util.parsing.combinator._
import scala.util.parsing.input._
import scalaz.Memo._

/**
  * Created by ichikawa on 2017/06/20.
  */
trait ContextSensitiveParsersModule {
  this: Environments =>

  trait ContextSensitiveParsers {
    self =>

    def delimiter: ContextFreeScanner[Any]

    type ~ [+A, +B] = Impl.~[A, B]
    val ~ : Impl.~.type = Impl.~

    def accept [T] (kind: String, f: PartialFunction[Char, T]): ContextFreeScanner[T] = new ContextFreeScanner(Impl.accept(kind, f))
    def elem (e: Char): ContextFreeScanner[Char] = new ContextFreeScanner(Impl.elem(e))
    def elem (kind: String, f: Char => Boolean): ContextFreeScanner[Char] = new ContextFreeScanner(Impl.elem(kind, f))
    def elem [E <: Char] (implicit Tag: ClassTag[E]): ContextFreeScanner[E] = new ContextFreeScanner(Impl.accept(Tag.toString(), { case Tag(x) => x }))

    def regex (r: Regex): ContextFreeScanner[String] = new ContextFreeScanner(Impl.regex(r))
    def word (s: String): ContextFreeScanner[String] = new ContextFreeScanner(Impl.literal(s))

    def positioned [T <: Positional : HasEnvModify] (parser: ContextSensitiveParser[T]): ContextSensitiveParser[T] = new ContextSensitiveParser(env => Impl.positioned(parser.parser(env)))

    object ContextSensitiveParser {
      def success [T : HasEnvModify] (t: => T): ContextSensitiveParser[T] = new ContextSensitiveParser(_ => Impl.success(t))
      //def failure (msg: => String): ContextSensitiveParser[Nothing] = new ContextSensitiveParser[Nothing](_ => Impl.failure(msg))
    }

    object ContextSensitiveScanner {
      def success [T : HasEnvModify] (t: => T): ContextSensitiveScanner[T] = new ContextSensitiveScanner(_ => Impl.success(t))
      //def failure (msg: => String): ContextSensitiveScanner[Nothing] = new ContextSensitiveScanner[Nothing](_ => Impl.failure(msg))
    }

    class ContextSensitiveParser [+T : HasEnvModify] private[ContextSensitiveParsers] (getParser: Environment => Impl.PackratParser[T]) {
      private [ContextSensitiveParsers] val parser: Environment => Impl.PackratParser[T] = mutableHashMapMemo(getParser)
      private [ContextSensitiveParsers] def apply (in: Reader[Char], env: Environment): Impl.ParseResult[T] = (delimiter.parser ~> parser(env) <~ delimiter.parser).apply(new Impl.PackratReader[Char](in))

      def apply (src: String, env: Environment): Try[T] = apply(new CharSequenceReader(src), env) match {
        case Impl.Success(result, _) => Success(result)
        case Impl.Failure(msg, _)    => Failure(ParseException(msg))
      }

      def ~ [U : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[T ~ U] = new ContextSensitiveParser(env => for {
        t <- this.parser(env) <~ delimiter.parser
        u <- that.parser(env.modify(t))
      } yield Impl.~.apply(t, u))

      def <~ (that: => ContextFreeParser[_]): ContextSensitiveParser[T] = new ContextSensitiveParser(env => this.parser(env) <~ delimiter.parser <~ that.parser)

      def | [U >: T : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser(env) | that.parser(env))
      def ||| [U >: T : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser(env) ||| that.parser(env))

      def ? : ContextSensitiveParser[Option[T]] = new ContextSensitiveParser(env => parser(env).?)

      def * : ContextSensitiveParser[List[T]] = this.+ | ContextSensitiveParser.success[List[T]](Nil)
      def * (sep: => ContextFreeParser[_]): ContextSensitiveParser[List[T]] = this.+(sep) | ContextSensitiveParser.success[List[T]](Nil)

      def + : ContextSensitiveParser[List[T]] = new ContextSensitiveParser(env => for {
        head <- this.parser(env) <~ delimiter.parser
        tail <- this.*.parser(env.modify(head))
      } yield head :: tail)

      def + (sep: => ContextFreeParser[_]): ContextSensitiveParser[List[T]] = new ContextSensitiveParser(env => for {
        head <- this.parser(env) <~ delimiter.parser
        _    <- sep.parser <~ delimiter.parser
        tail <- this.*(sep).parser(env.modify(head))
      } yield head :: tail)

      def map [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env).map(f))
      def flatMap [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => for {
        t <- parser(env) <~ delimiter.parser
        r <- f(t).parser(env.modify(t))
      } yield r)

      def depends [R : HasEnvModify] (f: Environment => ContextSensitiveParser[R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => f(env).parser(env))

      def into [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = flatMap(f)
      def >> [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = flatMap(f)
      def ^^ [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = map(f)
      def ^^^ [R : HasEnvModify] (f: => R): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env) ^^^ f)

      //def ~> [U : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser(env) ~> delimiter.parser ~> that.parser(env))
      //def & : ContextSensitiveParser[T] = new ContextSensitiveParser(env => Impl.guard(parser(env)))
      //def ! : ContextSensitiveParser[Unit] = new ContextSensitiveParser(env => Impl.not(parser(env)))
      //def ! (f: T => Boolean): ContextSensitiveParser[T] = new ContextSensitiveParser(env => parser(env) ^? { case r if ! f(r) => r })
      //def mapOption [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env) ^^ f ^? { case Some(r) => r })
      //def >>? [R : HasEnvModify] (f: T => Option[ContextSensitiveParser[R]]): ContextSensitiveParser[R] = flatMap(f(_).getOrElse(ContextSensitiveParser.failure("")))
      //def ^? [R : HasEnvModify] (f: PartialFunction[T, R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env) ^? f)
      //def ^^? [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveParser[R] = mapOption(f)
      /*def *! (f: List[T] => Boolean): ContextSensitiveParser[List[T]] = new ContextSensitiveParser(env => new Impl.Parser[List[T]] {
        def apply (in: Input): ParseResult[List[T]] = apply_rec(Nil, env, in)
        private def apply_rec (list: List[T], e: Environment, in: Input): ParseResult[List[T]] = parser(e)(in) match {
          case Impl.Success(res, next) =>
            val rs = list :+ res
            if (f(rs)) Impl.Success(list, in)
            else apply_rec(rs, e.modify(res), next)
          case _ => Impl.Success(list, in)
        }
      })*/

      def log (s: String): ContextSensitiveParser[T] = new ContextSensitiveParser(env => Impl.log(parser(env))(s))
    }

    class ContextFreeParser[+T] private[ContextSensitiveParsers] (getParser: => Impl.PackratParser[T]) {
      private[ContextSensitiveParsers] lazy val parser = getParser

      def ~ [U] (that: => ContextFreeParser[U]): ContextFreeParser[T ~ U] = new ContextFreeParser((this.parser <~ delimiter.parser) ~ that.parser)
      def ~> [U] (that: => ContextFreeParser[U]): ContextFreeParser[U] = new ContextFreeParser(this.parser ~> delimiter.parser ~> that.parser)
      def ~> [U : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser ~> delimiter.parser ~> that.parser(env))
      def <~ (that: => ContextFreeParser[_]): ContextFreeParser[T] = new ContextFreeParser(this.parser <~ delimiter.parser <~ that.parser)

      def | [U >: T] (that: ContextFreeParser[U]): ContextFreeParser[U] = new ContextFreeParser(this.parser | that.parser)
      def ||| [U >: T] (that: ContextFreeParser[U]): ContextFreeParser[U] = new ContextFreeParser(this.parser ||| that.parser)

      def ? : ContextFreeParser[Option[T]] = new ContextFreeParser(this.parser.?)
      def & : ContextFreeParser[T] = new ContextFreeParser(Impl.guard(this.parser))
      def ! : ContextFreeParser[Unit] = new ContextFreeParser(Impl.not(this.parser))

      def * : ContextFreeParser[List[T]] = new ContextFreeParser(Impl.repsep(this.parser, delimiter.parser))
      def + : ContextFreeParser[List[T]] = new ContextFreeParser(Impl.rep1sep(this.parser, delimiter.parser))
      def * (sep: ContextFreeParser[_]): ContextFreeParser[List[T]] = new ContextFreeParser(Impl.repsep(this.parser, delimiter.parser ~> sep.parser ~> delimiter.parser))
      def + (sep: ContextFreeParser[_]): ContextFreeParser[List[T]] = new ContextFreeParser(Impl.rep1sep(this.parser, delimiter.parser ~> sep.parser ~> delimiter.parser))

      def map [R] (f: T => R): ContextFreeParser[R] = new ContextFreeParser(this.parser.map(f))
      def map [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = new ContextSensitiveParser(_ => this.parser.map(f))
      def flatMap [R] (f: T => ContextFreeParser[R]): ContextFreeParser[R] = new ContextFreeParser((this.parser <~ delimiter.parser).flatMap(f(_).parser))

      def ^^ [R] (f: T => R): ContextFreeParser[R] = map(f)
      def ^^ [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = map(f)
      def ^^^ [R] (f: => R): ContextFreeParser[R] = new ContextFreeParser(this.parser ^^^ f)
      def ^^^ [R : HasEnvModify] (f: => R): ContextSensitiveParser[R] = new ContextSensitiveParser(_ => this.parser ^^^ f)
      def >> [R] (f: T => ContextFreeParser[R]): ContextFreeParser[R] = flatMap(f)
    }

    class ContextSensitiveScanner[+T : HasEnvModify] private[ContextSensitiveParsers] (getParser: Environment => Impl.PackratParser[T]) {
      private [ContextSensitiveParsers] val parser: Environment => Impl.PackratParser[T] = mutableHashMapMemo(getParser)

      lazy val ^ : ContextSensitiveParser[T] = new ContextSensitiveParser(parser)

      def ~ [U : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[T ~ U] = new ContextSensitiveScanner(env => for {
        t <- this.parser(env)
        u <- that.parser(env.modify(t))
      } yield Impl.~.apply(t, u))

      def <~ [_] (that: => ContextFreeScanner[_]): ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => this.parser(env) <~ that.parser)

      def | [U >: T : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser(env) | that.parser(env))
      def ||| [U >: T : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser(env) ||| that.parser(env))

      def * : ContextSensitiveScanner[List[T]] = this.+ | ContextSensitiveScanner.success[List[T]](Nil)
      def * (sep: => ContextFreeScanner[_]): ContextSensitiveScanner[List[T]] = this.+(sep) | ContextSensitiveScanner.success[List[T]](Nil)

      def + : ContextSensitiveScanner[List[T]] = new ContextSensitiveScanner(env => for {
        head <- this.parser(env)
        tail <- this.*.parser(env.modify(head))
      } yield head :: tail)

      def + (sep: => ContextFreeScanner[_]): ContextSensitiveScanner[List[T]] = new ContextSensitiveScanner(env => for {
        head <- this.parser(env)
        _    <- sep.parser
        tail <- this.*(sep).parser(env.modify(head))
      } yield head :: tail)

      def map [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env).map(f))

      def flatMap [R : HasEnvModify] (f: T => ContextSensitiveScanner[R]): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => for {
        t <- this.parser(env)
        r <- f(t).parser(env.modify(t))
      } yield r)

      def >> [R : HasEnvModify] (f: T => ContextSensitiveScanner[R]): ContextSensitiveScanner[R] = flatMap(f)

      def ^^ [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = map(f)
      def ^^^ [R : HasEnvModify] (f: => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env) ^^^ f)

      def log (s: String): ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => Impl.log(parser(env))(s))

      //def ~> [U : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser(env) ~> that.parser(env))
      //def ? : ContextSensitiveScanner[Option[T]] = new ContextSensitiveScanner(env => parser(env).?)
      //def & : ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => Impl.guard(parser(env)))
      //def ! : ContextSensitiveScanner[Unit] = new ContextSensitiveScanner(env => Impl.not(parser(env)))
      //def mapOption [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env) ^^ f ^? { case Some(r) => r })
      //def ^? [R : HasEnvModify] (f: PartialFunction[T, R]): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env) ^? f)
      //def ^^? [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveScanner[R] = mapOption(f)
    }

    class ContextFreeScanner[+T] private[ContextSensitiveParsers] (getParser: => Impl.PackratParser[T]) {
      private[ContextSensitiveParsers] lazy val parser = getParser

      lazy val ^ : ContextFreeParser[T] = new ContextFreeParser(parser)

      def ~ [U] (that: => ContextFreeScanner[U]): ContextFreeScanner[T ~ U] = new ContextFreeScanner((this.parser <~ delimiter.parser) ~ that.parser)
      def ~> [U] (that: => ContextFreeScanner[U]): ContextFreeScanner[U] = new ContextFreeScanner(this.parser ~> delimiter.parser ~> that.parser)
      def ~> [U : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser ~> delimiter.parser ~> that.parser(env))
      def <~ (that: => ContextFreeScanner[_]): ContextFreeScanner[T] = new ContextFreeScanner(this.parser <~ delimiter.parser <~ that.parser)

      def | [U >: T] (that: ContextFreeScanner[U]): ContextFreeScanner[U] = new ContextFreeScanner(this.parser | that.parser)
      def ||| [U >: T] (that: ContextFreeScanner[U]): ContextFreeScanner[U] = new ContextFreeScanner(this.parser ||| that.parser)

      def ? : ContextFreeScanner[Option[T]] = new ContextFreeScanner(this.parser.?)
      def & : ContextFreeScanner[T] = new ContextFreeScanner(Impl.guard(this.parser))
      def ! : ContextFreeScanner[Unit] = new ContextFreeScanner(Impl.not(this.parser))

      def * : ContextFreeScanner[List[T]] = new ContextFreeScanner(Impl.repsep(this.parser, delimiter.parser))
      def + : ContextFreeScanner[List[T]] = new ContextFreeScanner(Impl.rep1sep(this.parser, delimiter.parser))
      def * (sep: ContextFreeScanner[_]): ContextFreeScanner[List[T]] = new ContextFreeScanner(Impl.repsep(this.parser, delimiter.parser ~> sep.parser ~> delimiter.parser))
      def + (sep: ContextFreeScanner[_]): ContextFreeScanner[List[T]] = new ContextFreeScanner(Impl.rep1sep(this.parser, delimiter.parser ~> sep.parser ~> delimiter.parser))

      def map [R] (f: T => R): ContextFreeScanner[R] = new ContextFreeScanner(this.parser.map(f))
      def map [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(_ => this.parser.map(f))
      def flatMap [R] (f: T => ContextFreeScanner[R]): ContextFreeScanner[R] = new ContextFreeScanner((this.parser <~ delimiter.parser).flatMap(f(_).parser))

      def ^^ [R] (f: T => R): ContextFreeScanner[R] = map(f)
      def ^^ [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = map(f)
      def ^^^ [R] (f: => R): ContextFreeScanner[R] = new ContextFreeScanner(this.parser ^^^ f)
      def ^^^ [R : HasEnvModify] (f: => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(_ => this.parser ^^^ f)
      def >> [R] (f: T => ContextFreeScanner[R]): ContextFreeScanner[R] = flatMap(f)
    }

    implicit class EnvWithModify (env: Environment) {
      def modify [T: HasEnvModify] (t: T) : Environment = implicitly[HasEnvModify[T]].modify(env, t)
    }

    trait HasEnvModify [T] {
      def modify (env: Environment, t: T): Environment
    }

    object HasEnvModify {
      implicit def listHasEnvModify[T : HasEnvModify]: HasEnvModify[List[T]] = (env, list) => list.foldLeft(env)(_.modify(_))
      implicit def sequenceHasEnvModify[T : HasEnvModify, U : HasEnvModify]: HasEnvModify[T ~ U] = (env, pair) => env.modify(pair._1).modify(pair._2)
      implicit def optionHasEnvModify[T : HasEnvModify]: HasEnvModify[Option[T]] = (env, opt) => opt.map(t => env.modify(t)).getOrElse(env)
    }

    private[ContextSensitiveParsers] object Impl extends PackratParsers with RegexParsers {
      override def skipWhitespace: Boolean = false
    }
  }
}
