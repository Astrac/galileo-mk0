package astrac.galileo.rk4

import cats._

trait Scalable[S] {
  def scale(s: S, factor: Double): S
  def half(s: S): S = scale(s, 0.5)
}

trait State[S, D, T] extends Scalable[S] {
  def derivative: Derivative[D, T]
  def fromDerivative(d: D, t: T): S
  def monoid: Monoid[S]
}

object State extends StateInstances

trait Derivative[D, T] extends Scalable[D] {
  def monoid: Monoid[D]
  def time: Time[T]
}

object Derivative extends DerivativeInstances

trait Time[T] extends Scalable[T] {
  def group: Group[T]
  def ordering: Ordering[T]
  def ratio(num: T, den: T): Double
}

object Time extends TimeInstances

trait Integrable[S, D, T] {
  implicit def state: State[S, D, T]
  implicit def derivative: Derivative[D, T]
  implicit def time: Time[T]
}

object Integrable {
  implicit def fromSDT[S, D, T](implicit st: State[S, D, T], dr: Derivative[D, T], tm: Time[T]): Integrable[S, D, T] = new Integrable[S, D, T] {
    override implicit def state = st
    override implicit def derivative = dr
    override implicit def time = tm
  }
}
