package astrac.galileo

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.{FlatSpec, Matchers}
import rk4.auto._
import shapeless.contrib.scalacheck._
import spire.std.double._

class Rk4Spec extends BaseSpec {

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

  it should "calculate an integral of a differential function over an arbitrary interval from a set of initial points" in {
    check(Prop.forAll(diffIntervalBoundGen, diffIntervalBoundGen, diffFnGen, diffInitialSetGen) { (start, end, fn, initial) =>
      val (desc, der, int) = fn
      val dt = math.abs((start - end) / diffSteps)
      val res = rk4.integral(der, initial).inInterval(start, end, dt).value
      val exp = if (start < end) initial.map(int(end, start, _)) else initial.map(-int(start, end, _))
      val variance = exp.map(e => 0.075 + math.abs(e * 0.01))

      res
        .zip(exp)
        .zip(variance)
        .map {
          case ((r, e), v) =>
            (r >= e - v && r <= e + v) :|
              (s"Calculated integral of `$desc` from $start to $end in $dt increments is: $r (expected: $e +- $v)")
        }
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)
    })
  }

  it should "produce an iterator of an integral of a differential function from an arbitrary point" in {
    check(Prop.forAll(diffIntervalBoundGen, diffFnGen, diffInitialGen) { (from, fn, initial) =>
      val (desc, der, int) = fn
      val dt = 0.001
      val res = rk4.integral(der, initial).iterator(from, dt)

      res
        .map(checkDiffResult(desc, from, dt, initial, int))
        .take(1000)
        .toList
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)
    })
  }
}
