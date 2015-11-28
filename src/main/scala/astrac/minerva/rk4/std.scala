package astrac.minerva.rk4

import cats._

trait CommonInstances {
  implicit val doubleInstance = new Group[Double] {
    def inverse(a: Double): Double = -a
    val empty: Double = 0
    def combine(x: Double, y: Double): Double = x + y
  }

  trait DoubleScalable extends Scalable[Double] {
    def scale(n: Double, factor: Double) = n * factor
  }
}

trait TimeInstances extends CommonInstances {
  implicit lazy val doubleTime = new Time[Double] with DoubleScalable {
    val group = doubleInstance
    val ordering = implicitly[Ordering[Double]]
    def ratio(num: Double, den: Double) = num / den
  }
}

trait LowPriorityDerivateInstances extends CommonInstances {
  implicit def doubleDerivateGen[T](implicit tm: Time[T]) =
    new Derivate[Double, T] with DoubleScalable {
      val monoid = doubleInstance
      val time = tm
    }
}

trait DerivateInstances extends LowPriorityDerivateInstances {
  implicit lazy val doubleDerivate =
    new Derivate[Double, Double] with DoubleScalable {
      val monoid = doubleInstance
      val time = implicitly[Time[Double]]
    }
}

trait StateInstances extends CommonInstances {
  implicit lazy val doubleState =
    new State[Double, Double, Double] with DoubleScalable {
      val derivate = implicitly[Derivate[Double, Double]]
      def fromDerivate(d: Double, t: Double) = d * t
      val monoid = doubleInstance
    }
}
