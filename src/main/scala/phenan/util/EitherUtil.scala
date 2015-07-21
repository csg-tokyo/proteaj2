package phenan.util

import scala.language.implicitConversions

object EitherUtil {
  type | [+A, +B] = Either[A, B]

  implicit def eitherUtil [T] (v: T): EitherUtil[T] = new EitherUtil[T](v)
}

class EitherUtil[T] (val v: T) extends AnyVal {
  def l : Either[T, Nothing] = Left(v)
  def r : Either[Nothing, T] = Right(v)
}
