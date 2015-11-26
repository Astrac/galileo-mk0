package astrac.minerva.rk4

import cats._

trait State[S, D, T] {
  def semigroup: Semigroup[S]
  def derivate: Derivate[D, T]
  def scale(s: S, factor: Double): S
  def fromDerivate(d: D, t: T): S
}

trait Derivate[D, T] {
  def monoid: Monoid[D]
  def scale(d: D, factor: Double): D
  def time: Time[T]
}

trait Time[T] {
  def monoid: Monoid[T]
  def ordering: Ordering[T]
  def half(t: T): T
  def negate(t: T): T
  def ratio(num: T, den: T): Double
}

trait Integrable[S, D, T] {
  implicit def state: State[S, D, T]
  implicit def derivate: Derivate[D, T]
  implicit def time: Time[T]
}

object Integrable {
  implicit def fromSDT[S, D, T](implicit st: State[S, D, T], dr: Derivate[D, T], tm: Time[T]): Integrable[S, D, T] = new Integrable[S, D, T] {
    override implicit def state = st
    override implicit def derivate = dr
    override implicit def time = tm
  }
}
