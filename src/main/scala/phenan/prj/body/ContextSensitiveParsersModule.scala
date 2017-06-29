package phenan.prj.body

import phenan.prj._
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
  this: Environments with NameResolvers with EnvModifyStrategy with IRs with IRStatements with IRExpressions with JModules =>

  type ~ [+A, +B] = Impl.~[A, B]
  val ~ : Impl.~.type = Impl.~

  trait ContextSensitiveParsers {
    def baseEnvironment: BaseEnvironment
    def declaringModule: IRModule = baseEnvironment.declaringModule
    def thisType: Option[JObjectType] = baseEnvironment.thisType
    def resolver: NameResolver = baseEnvironment.resolver
    def highestPriority: Option[JPriority] = baseEnvironment.highestPriority
    def nextPriority(priority: JPriority): Option[JPriority] = baseEnvironment.nextPriority(priority)

    def delimiter: ContextFreeScanner[Any]

    def accept [T] (kind: String, f: PartialFunction[Char, T]): ContextFreeScanner[T] = new ContextFreeScanner(Impl.accept(kind, f))
    def elem (e: Char): ContextFreeScanner[Char] = new ContextFreeScanner(Impl.elem(e))
    def elem (kind: String, f: Char => Boolean): ContextFreeScanner[Char] = new ContextFreeScanner(Impl.elem(kind, f))
    def elem [E <: Char] (implicit Tag: ClassTag[E]): ContextFreeScanner[E] = new ContextFreeScanner(Impl.accept(Tag.toString(), { case Tag(x) => x }))

    def regex (r: Regex): ContextFreeScanner[String] = new ContextFreeScanner(Impl.regex(r))
    def word (s: String): ContextFreeScanner[String] = new ContextFreeScanner(Impl.literal(s))

    def parserEnvironment: ContextSensitiveParser[Environment] = new ContextSensitiveParser(env => ContextFreeParser.success(env))
    def scannerEnvironment: ContextSensitiveScanner[Environment] = new ContextSensitiveScanner(env => ContextFreeScanner.success(env))

    //def positioned [T <: Positional : HasEnvModify] (parser: ContextSensitiveParser[T]): ContextSensitiveParser[T] = new ContextSensitiveParser(env => Impl.positioned(parser.parser(env)))

    object ContextSensitiveParser {
      def apply [T : HasEnvModify] (f: Environment => ContextSensitiveParser[T]): ContextSensitiveParser[T] = new ContextSensitiveParser(env => f(env).parser(env))
      def success [T : HasEnvModify] (t: => T): ContextSensitiveParser[T] = new ContextSensitiveParser(_ => ContextFreeParser.success(t))
      def failure [T : HasEnvModify] (msg: => String): ContextSensitiveParser[T] = new ContextSensitiveParser[T](_ => ContextFreeParser.failure(msg))
    }

    object ContextFreeParser {
      def success [T] (t: => T): ContextFreeParser[T] = new ContextFreeParser(Impl.success(t))
      def failure (msg: => String): ContextFreeParser[Nothing] = new ContextFreeParser(Impl.failure(msg))
    }

    object ContextSensitiveScanner {
      def apply [T : HasEnvModify] (f: Environment => ContextSensitiveScanner[T]): ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => f(env).parser(env))
      def success [T : HasEnvModify] (t: => T): ContextSensitiveScanner[T] = new ContextSensitiveScanner(_ => ContextFreeScanner.success(t))
      def failure [T : HasEnvModify] (msg: => String): ContextSensitiveScanner[T] = new ContextSensitiveScanner[T](_ => ContextFreeScanner.failure(msg))
    }

    object ContextFreeScanner {
      def success [T] (t: => T): ContextFreeScanner[T] = new ContextFreeScanner(Impl.success(t))
      def failure (msg: => String): ContextFreeScanner[Nothing] = new ContextFreeScanner(Impl.failure(msg))
    }

    class ContextSensitiveParser [+T : HasEnvModify] private[ContextSensitiveParsers] (getParser: Environment => ContextFreeParser[T]) {
      private [ContextSensitiveParsers] val parser: Environment => ContextFreeParser[T] = mutableHashMapMemo(getParser)

      def apply (src: String, env: Environment): Try[T] = parser(env)(new CharSequenceReader(src)) match {
        case Impl.Success(result, _) => Success(result)
        case Impl.Failure(msg, _)    => Failure(ParseException(msg))
      }

      def ~ [U : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[T ~ U] = new ContextSensitiveParser(env => for {
        t <- this.parser(env)
        u <- that.parser(env.modify(t))
      } yield Impl.~.apply(t, u))

      def <~ (that: => ContextFreeParser[_]): ContextSensitiveParser[T] = new ContextSensitiveParser(env => this.parser(env) <~ that)

      def ~> [U : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser(env) ~> that.parser(env))

      def | [U >: T : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser(env) | that.parser(env))
      def ||| [U >: T : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this.parser(env) ||| that.parser(env))

      def ? : ContextSensitiveParser[Option[T]] = new ContextSensitiveParser(env => parser(env).?)
      def & : ContextSensitiveParser[T] = new ContextSensitiveParser(env => parser(env).&)
      def ! : ContextSensitiveParser[Unit] = new ContextSensitiveParser(env => parser(env).!)

      def * : ContextSensitiveParser[List[T]] = this.+ | ContextSensitiveParser.success[List[T]](Nil)
      def * (sep: => ContextFreeParser[_]): ContextSensitiveParser[List[T]] = this.+(sep) | ContextSensitiveParser.success[List[T]](Nil)

      def + : ContextSensitiveParser[List[T]] = new ContextSensitiveParser(env => for {
        head <- this.parser(env)
        tail <- this.*.parser(env.modify(head))
      } yield head :: tail)

      def + (sep: => ContextFreeParser[_]): ContextSensitiveParser[List[T]] = new ContextSensitiveParser(env => for {
        head <- this.parser(env)
        tail <- (sep ~> this.+(sep).parser(env.modify(head))).?
      } yield head :: tail.getOrElse(Nil))

      def map [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env).map(f))
      def mapOption [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env) ^^ f ^? { case Some(r) => r })

      def filter (f: T => Boolean): ContextSensitiveParser[T] = new ContextSensitiveParser(env => this.parser(env).filter(f))
      def filterEnv (f: (T, Environment) => Boolean): ContextSensitiveParser[T] = new ContextSensitiveParser(env => this.parser(env).filter(f(_, env)))

      def flatMap [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => for {
        t <- parser(env)
        r <- f(t).parser(env.modify(t))
      } yield r)

      def into [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = flatMap(f)
      def >> [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = flatMap(f)

      def ^? [R : HasEnvModify] (f: PartialFunction[T, R]): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env) ^? f)
      def ^^ [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = map(f)
      def ^^? [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveParser[R] = mapOption(f)
      def ^^^ [R : HasEnvModify] (f: => R): ContextSensitiveParser[R] = new ContextSensitiveParser(env => parser(env) ^^^ f)

      def log (s: String): ContextSensitiveParser[T] = new ContextSensitiveParser(env => parser(env).log(s))
    }

    implicit class ContextSensitiveParser_IRExpression (parser: ContextSensitiveParser[IRExpression]) {
      def withLocalContexts (contexts: List[IRContextRef]): ContextSensitiveParser[IRExpression] = new ContextSensitiveParser ( env =>
        parser.parser(env.withContexts(contexts, Nil)) ^^ { _.withContexts(contexts) }
      )
    }

    implicit class ContextSensitiveParser_IRStatement [T <: IRStatement] (parser: ContextSensitiveParser[T]) {
      def withLocalVariable (declaration: IRLocalDeclaration): ContextSensitiveParser[T] = new ContextSensitiveParser ( env =>
        parser.parser(env.defineLocals(declaration))
      )
    }

    class ContextFreeParser[+T] private[ContextSensitiveParsers] (getParser: => Impl.PackratParser[T]) {
      private[ContextSensitiveParsers] lazy val parser = getParser

      def apply (in: Reader[Char]): Impl.ParseResult[T] = (delimiter.parser ~> parser <~ delimiter.parser).apply(new Impl.PackratReader[Char](in))

      def ~ [U] (that: => ContextFreeParser[U]): ContextFreeParser[T ~ U] = new ContextFreeParser((this.parser <~ delimiter.parser) ~ that.parser)
      def ~> [U] (that: => ContextFreeParser[U]): ContextFreeParser[U] = new ContextFreeParser(this.parser ~> delimiter.parser ~> that.parser)
      def ~> [U : HasEnvModify] (that: => ContextSensitiveParser[U]): ContextSensitiveParser[U] = new ContextSensitiveParser(env => this ~> that.parser(env))
      def <~ (that: => ContextFreeParser[_]): ContextFreeParser[T] = new ContextFreeParser(this.parser <~ delimiter.parser <~ that.parser)

      def ? : ContextFreeParser[Option[T]] = new ContextFreeParser(this.parser.?)
      def & : ContextFreeParser[T] = new ContextFreeParser(Impl.guard(this.parser))
      def ! : ContextFreeParser[Unit] = new ContextFreeParser(Impl.not(this.parser))

      def * : ContextFreeParser[List[T]] = new ContextFreeParser(Impl.repsep(this.parser, delimiter.parser))
      def + : ContextFreeParser[List[T]] = new ContextFreeParser(Impl.rep1sep(this.parser, delimiter.parser))
      def * (sep: => ContextFreeParser[_]): ContextFreeParser[List[T]] = new ContextFreeParser(Impl.repsep(this.parser, delimiter.parser ~> sep.parser ~> delimiter.parser))
      def + (sep: => ContextFreeParser[_]): ContextFreeParser[List[T]] = new ContextFreeParser(Impl.rep1sep(this.parser, delimiter.parser ~> sep.parser ~> delimiter.parser))

      def map [R] (f: T => R): ContextFreeParser[R] = new ContextFreeParser(this.parser.map(f))
      def mapOption [R] (f: T => Option[R]): ContextFreeParser[R] = new ContextFreeParser(this.parser ^^ f ^? { case Some(r) => r })
      def flatMap [R] (f: T => ContextFreeParser[R]): ContextFreeParser[R] = new ContextFreeParser((this.parser <~ delimiter.parser).flatMap(f(_).parser))

      def mapS [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = new ContextSensitiveParser(_ => map(f))

      def filter (f: T => Boolean): ContextFreeParser[T] = new ContextFreeParser(this.parser.filter(f))

      def ^? [R] (f: PartialFunction[T, R]): ContextFreeParser[R] = new ContextFreeParser(this.parser ^? f)
      def ^^ [R] (f: T => R): ContextFreeParser[R] = map(f)
      def ^^? [R] (f: T => Option[R]): ContextFreeParser[R] = mapOption(f)
      def ^^# [R : HasEnvModify] (f: T => R): ContextSensitiveParser[R] = mapS(f)
      def ^^?# [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveParser[R] = new ContextSensitiveParser(_ => mapOption(f))

      def ^^^ [R] (f: => R): ContextFreeParser[R] = new ContextFreeParser(this.parser ^^^ f)
      def ^^^# [R : HasEnvModify] (f: => R): ContextSensitiveParser[R] = new ContextSensitiveParser(_ => this ^^^ f)
      def >> [R] (f: T => ContextFreeParser[R]): ContextFreeParser[R] = flatMap(f)

      def >># [R : HasEnvModify] (f: T => ContextSensitiveParser[R]): ContextSensitiveParser[R] = {
        new ContextSensitiveParser(env => flatMap(t => f(t).parser(env)))
      }

      def log (s: String): ContextFreeParser[T] = new ContextFreeParser(Impl.log(parser)(s))

      def repFor [U] (init: U, cond: U => Boolean, update: (U, T) => U): ContextFreeParser[U] = {
        map(update(init, _)).filter(cond) >> { repFor(_, cond, update) } | ContextFreeParser.success(init)
      }
    }

    implicit class ContextFreeParserLiftOp [T : HasEnvModify] (self: => ContextFreeParser[T]) {
      def ^# : ContextSensitiveParser[T] = new ContextSensitiveParser(_ => self)
    }

    implicit class ContextFreeParserChoiceOps [T] (self: => ContextFreeParser[T]) {
      def | [U >: T] (that: => ContextFreeParser[U]): ContextFreeParser[U] = new ContextFreeParser(self.parser | that.parser)
      def ||| [U >: T] (that: => ContextFreeParser[U]): ContextFreeParser[U] = new ContextFreeParser(self.parser ||| that.parser)
    }

    class ContextSensitiveScanner[+T : HasEnvModify] private[ContextSensitiveParsers] (getParser: Environment => ContextFreeScanner[T]) {
      private [ContextSensitiveParsers] val parser: Environment => ContextFreeScanner[T] = mutableHashMapMemo(getParser)

      lazy val ^ : ContextSensitiveParser[T] = new ContextSensitiveParser(env => parser(env).^)

      def ~ [U : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[T ~ U] = new ContextSensitiveScanner(env => for {
        t <- this.parser(env)
        u <- that.parser(env.modify(t))
      } yield Impl.~.apply(t, u))

      def <~ [_] (that: => ContextFreeScanner[_]): ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => this.parser(env) <~ that)

      def ~> [U : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser(env) ~> that.parser(env))

      def | [U >: T : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser(env) | that.parser(env))
      def ||| [U >: T : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this.parser(env) ||| that.parser(env))

      def ? : ContextSensitiveScanner[Option[T]] = new ContextSensitiveScanner(env => parser(env).?)
      def & : ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => parser(env).&)
      def ! : ContextSensitiveScanner[Unit] = new ContextSensitiveScanner(env => parser(env).!)

      def * : ContextSensitiveScanner[List[T]] = this.+ | ContextSensitiveScanner.success[List[T]](Nil)
      def * (sep: => ContextFreeScanner[_]): ContextSensitiveScanner[List[T]] = this.+(sep) | ContextSensitiveScanner.success[List[T]](Nil)

      def + : ContextSensitiveScanner[List[T]] = new ContextSensitiveScanner(env => for {
        head <- this.parser(env)
        tail <- this.*.parser(env.modify(head))
      } yield head :: tail)

      def + (sep: => ContextFreeScanner[_]): ContextSensitiveScanner[List[T]] = new ContextSensitiveScanner[List[T]](env => for {
        head <- this.parser(env)
        _    <- sep
        tail <- this.*(sep).parser(env.modify(head))
      } yield head :: tail)

      def map [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env).map(f))
      def mapOption [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env) ^^ f ^? { case Some(r) => r })

      def flatMap [R : HasEnvModify] (f: T => ContextSensitiveScanner[R]): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => for {
        t <- this.parser(env)
        r <- f(t).parser(env.modify(t))
      } yield r)

      def >> [R : HasEnvModify] (f: T => ContextSensitiveScanner[R]): ContextSensitiveScanner[R] = flatMap(f)

      def ^? [R : HasEnvModify] (f: PartialFunction[T, R]): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env) ^? f)
      def ^^ [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = map(f)
      def ^^? [R : HasEnvModify] (f: T => Option[R]): ContextSensitiveScanner[R] = mapOption(f)
      def ^^^ [R : HasEnvModify] (f: => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(env => parser(env) ^^^ f)

      def log (s: String): ContextSensitiveScanner[T] = new ContextSensitiveScanner(env => parser(env).log(s))
    }

    implicit class ContextSensitiveScanner_IRExpression (scanner: ContextSensitiveScanner[IRExpression]) {
      def withLocalContexts(contexts: List[IRContextRef]): ContextSensitiveScanner[IRExpression] = new ContextSensitiveScanner (env =>
        scanner.parser(env.withContexts(contexts, Nil)) ^^ { _.withContexts(contexts) }
      )
    }

    class ContextFreeScanner[+T] private[ContextSensitiveParsers] (getParser: => Impl.PackratParser[T]) {
      private[ContextSensitiveParsers] lazy val parser = getParser

      lazy val ^ : ContextFreeParser[T] = new ContextFreeParser(parser)

      def ~ [U] (that: => ContextFreeScanner[U]): ContextFreeScanner[T ~ U] = new ContextFreeScanner(this.parser ~ that.parser)
      def ~> [U] (that: => ContextFreeScanner[U]): ContextFreeScanner[U] = new ContextFreeScanner(this.parser ~> that.parser)
      def ~> [U : HasEnvModify] (that: => ContextSensitiveScanner[U]): ContextSensitiveScanner[U] = new ContextSensitiveScanner(env => this ~> that.parser(env))
      def <~ (that: => ContextFreeScanner[_]): ContextFreeScanner[T] = new ContextFreeScanner(this.parser <~ that.parser)

      def ? : ContextFreeScanner[Option[T]] = new ContextFreeScanner(this.parser.?)
      def & : ContextFreeScanner[T] = new ContextFreeScanner(Impl.guard(this.parser))
      def ! : ContextFreeScanner[Unit] = new ContextFreeScanner(Impl.not(this.parser))

      def * : ContextFreeScanner[List[T]] = new ContextFreeScanner(this.parser.*)
      def + : ContextFreeScanner[List[T]] = new ContextFreeScanner(this.parser.+)
      def * (sep: ContextFreeScanner[_]): ContextFreeScanner[List[T]] = new ContextFreeScanner(Impl.repsep(this.parser, sep.parser))
      def + (sep: ContextFreeScanner[_]): ContextFreeScanner[List[T]] = new ContextFreeScanner(Impl.rep1sep(this.parser, sep.parser))

      def map [R] (f: T => R): ContextFreeScanner[R] = new ContextFreeScanner(this.parser.map(f))
      def flatMap [R] (f: T => ContextFreeScanner[R]): ContextFreeScanner[R] = new ContextFreeScanner(this.parser.flatMap(f(_).parser))

      def mapS [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(_ => map(f))

      def ^? [R] (f: PartialFunction[T, R]): ContextFreeScanner[R] = new ContextFreeScanner(this.parser ^? f)
      def ^^ [R] (f: T => R): ContextFreeScanner[R] = map(f)
      def ^^# [R : HasEnvModify] (f: T => R): ContextSensitiveScanner[R] = mapS(f)
      def ^^^ [R] (f: => R): ContextFreeScanner[R] = new ContextFreeScanner(this.parser ^^^ f)
      def ^^^# [R : HasEnvModify] (f: => R): ContextSensitiveScanner[R] = new ContextSensitiveScanner(_ => this ^^^ f)
      def >> [R] (f: T => ContextFreeScanner[R]): ContextFreeScanner[R] = flatMap(f)

      def log (s: String): ContextFreeScanner[T] = new ContextFreeScanner(Impl.log(parser)(s))
    }

    implicit class ContextFreeScannerLiftOp [T : HasEnvModify] (self: => ContextFreeScanner[T]) {
      def ^# : ContextSensitiveScanner[T] = new ContextSensitiveScanner(_ => self)
    }

    implicit class ContextFreeScannerChoiceOps [T] (self: => ContextFreeScanner[T]) {
      def | [U >: T] (that: => ContextFreeScanner[U]): ContextFreeScanner[U] = new ContextFreeScanner(self.parser | that.parser)
      def ||| [U >: T] (that: => ContextFreeScanner[U]): ContextFreeScanner[U] = new ContextFreeScanner(self.parser ||| that.parser)
    }
  }

  protected object Impl extends RegexParsers with PackratParsers {
    override def skipWhitespace: Boolean = false
  }
}
