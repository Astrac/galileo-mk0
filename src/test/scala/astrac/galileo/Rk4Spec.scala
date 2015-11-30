package astrac.galileo

import org.scalatest.{FlatSpec, Matchers}
import rk4._

class Rk4Spec extends FlatSpec with Matchers {
  val minDt = 0.001
  val maxDt = 0.25

  def integrate(fn: Double => Double, samples: Iterable[Double]) =
    integrator(fn).compute(samples, minDt, maxDt)

  "The RK4 integration" should "integrate `f(x) = 1` in [0, 1] giving 1" in {
    integrate(x => 1, 0.0 to 1.0 by minDt).last should equal(1.0 +- 0.05)
  }

  it should "integrate `f(x) = x` in [0, 1] giving 0.5" in {
    integrate(x => x, 0.0 to 1.0 by minDt).last should equal(0.5 +- 0.05)
  }

  it should "integrate `f(x) = x + 2` in [0, 1] giving 2.5" in {
    integrate(x => x + 2, 0.0 to 1.0 by minDt).last should equal(2.5 +- 0.05)
  }

  it should "integrate `f(x) = x` in [1, 2] giving 1.5" in {
    integrate(x => x, 1.0 to 2.0 by minDt).last should equal(1.5 +- 0.05)
  }

  it should "integrate `f(x) = x + 2` in [1, 3] giving 8" in {
    integrate(x => x + 2, 1.0 to 3.0 by minDt).last should equal(8.0 +- 0.05)
  }

  it should "integrate `f(x) = sin x` in [π, π] giving 0" in {
    integrate(x => math.sin(x), (- math.Pi) to math.Pi by minDt).last should equal(0.0 +- 0.05)
  }

  it should "integrate `f(x) = x^2` in [1, 3] giving 26/3" in {
    integrate(x => x * x, 1.0 to 3.0 by minDt).last should equal((26.0 / 3.0) +- 0.05)
  }

  it should "produce an empty result for an empty sample list" in {
    integrate(x => x, Nil) should be(empty)
  }

  it should "produce a result with an empty state for a single sample" in {
    integrate(x => x, 1.0 :: Nil) should equal(0.0 :: Nil)
  }
}
