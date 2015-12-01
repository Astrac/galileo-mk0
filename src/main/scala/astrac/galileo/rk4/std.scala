package astrac.galileo.rk4

import cats.Group
import cats.Monoid
import cats.std.double._

trait CommonInstances {
  trait DoubleScalable extends Scalable[Double] {
    def scale(n: Double, factor: Double) = n * factor
  }
}

trait TimeInstances extends CommonInstances {
  implicit lazy val doubleTime = new Time[Double] with DoubleScalable {
    val group = implicitly[Group[Double]]
    val ordering = implicitly[Ordering[Double]]
    def ratio(num: Double, den: Double) = num / den
  }
}

trait DerivativeInstances extends CommonInstances {
  implicit lazy val doubleDerivative =
    new Derivative[Double, Double] with DoubleScalable {
      val monoid = implicitly[Group[Double]]
      val time = implicitly[Time[Double]]
    }
}

trait StateInstances extends CommonInstances {
  implicit lazy val doubleState =
    new State[Double, Double, Double] with DoubleScalable {
      val derivative = implicitly[Derivative[Double, Double]]
      def fromDerivative(d: Double, t: Double) = d * t
      val monoid = implicitly[Monoid[Double]]
    }
}
