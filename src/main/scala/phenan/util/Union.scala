package phenan.util

sealed trait Union

sealed trait :|: [+L, +R <: Union] extends Union

final case class -:|: [+L, +R <: Union] (l: L) extends :|: [L, R]

final case class :|:- [+L, +R <: Union] (r: R) extends :|: [L, R]

sealed trait UNil extends Union

object Union {
  def apply[C <: Union] = new UnionFactory[C]

  class UnionFactory[C <: Union] {
    def apply[T](t: T)(implicit inject: Inject[C, T]): C = inject(t)
  }
}

trait Inject[C <: Union, T] {
  def apply (t: T): C
}

object Inject {
  implicit def leftInject [L, R <: Union]: Inject[L :|: R, L] = new Inject[L :|: R, L] {
    def apply (l: L): L :|: R = -:|:(l)
  }
  implicit def rightInject [L, R <: Union, T] (implicit inject: Inject[R, T]): Inject[L :|: R, T] = new Inject[L :|: R, T] {
    def apply (t: T): L :|: R = :|:-(inject(t))
  }
}