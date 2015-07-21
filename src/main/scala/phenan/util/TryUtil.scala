package phenan.util

import scala.util.Try

import scalaz._

object TryUtil {
  implicit val tryInstance: Monad[Try] = new Monad[Try] {
    override def point[A](a: => A): Try[A] = Try(a)
    override def bind[A, B](fa: Try[A])(f: (A) => Try[B]): Try[B] = fa.flatMap(f)
  }
}
