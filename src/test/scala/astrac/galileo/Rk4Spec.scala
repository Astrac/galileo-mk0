package astrac.galileo

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.Checkers
import rk4.auto._
import shapeless.contrib.scalacheck._
import spire.std.double._

class Rk4Spec extends FlatSpec with Matchers with Checkers {
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

  "The RK4 integration" should "calculate an integral of a simple function over an arbitrary interval" in {
    check(Prop.forAll(intervalBoundGen, intervalBoundGen, simpleFnGen) { (start, end, fn) =>
      val (desc, der, int) = fn
      val dt = math.abs((start - end) / steps)
      val res = rk4.integral(der).inInterval(start, end, dt).value
      val exp = int(end) - int(start)
      val variance = 0.05 + math.abs(exp * 0.01)

      (res >= exp - variance && res <= exp + variance) :|
        (s"Calculated integral of `$desc` from $start to $end in $dt increments is: $res (expected: $exp +- $variance)")
    })
  }

  it should "produce an iterator of an integral of a simple function from an arbitrary point" in {
    check(Prop.forAll(intervalBoundGen, simpleFnGen) { (from, fn) =>
      val (desc, der, int) = fn
      val dt = 0.001
      val res = rk4.integral(der).iterator(from, dt)

      res
        .map {
          case rk4.Integral.Result(v, t) =>
            val exp = int(t) - int(from)
            val variance = math.abs((t - from) * 0.01) + math.abs(exp * 0.01)
            (v >= exp - variance && v <= exp + variance) :|
              (s"Iterated integral of `$desc` from $from in $dt increments is: $v at $t (expected: $exp +- $variance)")
        }
        .take(1000)
        .toList
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)
    })
  }

  it should "calculate an integral of a differential function over an arbitrary interval" in {
    check(Prop.forAll(diffIntervalBoundGen, diffIntervalBoundGen, diffFnGen, diffInitialGen) { (start, end, fn, initial) =>
      val (desc, der, int) = fn
      val dt = math.abs((start - end) / diffSteps)
      val res = rk4.integral(der, initial).inInterval(start, end, dt).value
      val exp = if (start < end) int(end, start, initial) else -int(start, end, initial)
      val variance = 0.05 + math.abs(exp * 0.01)

      (res >= exp - variance && res <= exp + variance) :|
        (s"Calculated integral of `$desc` from $start to $end in $dt increments is: $res (expected: $exp +- $variance)")
    })
  }

  it should "produce an iterator of an integral of a differential function from an arbitrary point" in {
    check(Prop.forAll(diffIntervalBoundGen, diffFnGen, diffInitialGen) { (from, fn, initial) =>
      val (desc, der, int) = fn
      val dt = 0.001
      val res = rk4.integral(der, initial).iterator(from, dt)

      res
        .map {
          case rk4.Integral.Result(v, t) =>
            val exp = int(t, from, initial)
            val variance = 0.05 + math.abs(exp * 0.01)
            (v >= exp - variance && v <= exp + variance) :|
              (s"Iterated integral of `$desc` from $from in $dt increments is: $v at $t (expected: $exp +- $variance)")
        }
        .take(1000)
        .toList
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)
    })
  }
}
