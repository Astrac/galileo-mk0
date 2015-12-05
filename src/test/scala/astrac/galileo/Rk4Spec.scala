package astrac.galileo

import org.scalatest.{FlatSpec, Matchers}
import spire.std.double._
import rk4.auto._

class Rk4Spec extends FlatSpec with Matchers {
  val dt = 0.001

  def integrate(fn: Double => Double, from: Double, to: Double) = {
    rk4.integral(fn).inInterval(from, to, dt).value
  }

  "The RK4 integration" should "integrate `f(x) = 1` in [0, 1] giving 1" in {
    integrate(x => 1, 0.0, 1.0) should equal(1.0 +- 0.05)
  }

  it should "integrate `f(x) = x` in [0, 1] giving 0.5" in {
    integrate(x => x, 0.0, 1.0) should equal(0.5 +- 0.05)
  }

  it should "integrate `f(x) = x + 2` in [0, 1] giving 2.5" in {
    integrate(x => x + 2, 0.0, 1.0) should equal(2.5 +- 0.05)
  }

  it should "integrate `f(x) = x` in [1, 2] giving 1.5" in {
    integrate(x => x, 1.0, 2.0) should equal(1.5 +- 0.05)
  }

  it should "integrate `f(x) = x + 2` in [1, 3] giving 8" in {
    integrate(x => x + 2, 1.0, 3.0) should equal(8.0 +- 0.05)
  }

  it should "integrate `f(x) = sin x` in [π, π] giving 0" in {
    integrate(x => math.sin(x), (-math.Pi), math.Pi) should equal(0.0 +- 0.05)
  }

  it should "integrate `f(x) = x^2` in [1, 3] giving 26/3" in {
    integrate(x => x * x, 1.0, 3.0) should equal((26.0 / 3.0) +- 0.05)
  }

  it should "produce a result with an empty state for a single sample" in {
    integrate(x => x, 1.0, 1.0) should equal(0.0)
  }
}
