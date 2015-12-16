package astrac.galileo

import astrac.galileo.rk4.Integral.Result
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers

class BaseSpec extends FlatSpec with Matchers with Checkers {
  val steps = 10000.0
  val intervalBoundGen = Gen.choose(-15.0, 15.0)

  val diffSteps = 25000.0
  val diffIntervalBoundGen = Gen.choose(-5.0, 5.0)
  val diffInitialGen = Gen.choose(-10.0, 10.0)

  val simpleFnGen = Gen.oneOf(Seq[(String, Double => Double, Double => Double)](
    ("f(x) = 1", x => 1, x => x),
    ("f(x) = x", x => x, x => 0.5 * x * x),
    ("f(x) = x ^ 2", x => x * x, x => (1.0 / 3.0) * x * x * x),
    ("f(x) = x + 3", x => x + 3, x => 0.5 * x * x + 3 * x),
    ("f(x) = sin(x)", x => math.sin(x), x => -math.cos(x))
  ))

  // Description, derivative (s, t => d), integral (y(a) = b => y(x, a, b)) - thanks Wolfram Alpha
  val diffFnGen = Gen.oneOf(Seq[(String, (Double, Double) => Double, (Double, Double, Double) => Double)](
    ("y' = y - x", (y, x) => y - x, (x, a, b) => math.exp(-a) * (math.exp(x) * (b - a - 1) + math.exp(a) * (x + 1))),
    ("y' = y + e ^ x", (y, x) => y + math.exp(x), (x, a, b) => math.exp(x - a) * (math.exp(a) * (x - a) + b)),
    ("y' = 2 * y", (y, _) => 2 * y, (x, a, b) => b * math.exp(2 * x - 2 * a))
  ))

  def checkDiffResult(desc: String, from: Double, dt: Double, initial: Double, int: (Double, Double, Double) => Double)(result: Result[Double, Double]) = {
    val exp = int(result.at, from, initial)
    val variance = 0.05 + math.abs(exp * 0.01)
    (result.value >= exp - variance && result.value <= exp + variance) :|
      (s"Iterated integral of `$desc` from $from in $dt increments is: ${result.value} at ${result.at} (expected: $exp +- $variance)")
  }
}
