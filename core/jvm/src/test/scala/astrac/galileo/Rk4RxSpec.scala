package astrac.galileo

import astrac.galileo.rk4.Integral.ObservedResult
import astrac.galileo.rk4.Integral.Result
import monifu.concurrent.Implicits.globalScheduler
import monifu.reactive.Observable
import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.BooleanOperators
import org.scalatest.{FlatSpec, Matchers}
import rk4.auto._
import scala.concurrent.Await
import scala.concurrent.duration._
import shapeless.contrib.scalacheck._
import spire.std.double._

class Rk4RxSpec extends BaseSpec {

  "The RK4 RX integrator" should "produce an observable of the integration of a simulation" in {
    check(Prop.forAll(diffIntervalBoundGen, diffFnGen, diffInitialGen) { (from, fn, initial) =>
      val (desc, der, int) = fn
      val dt = 0.001
      val res = rk4
        .integral(der, initial)
        .observable(from, dt)
        .map(checkDiffResult(desc, from, dt, initial, int))
        .take(1000)
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)

      Await.result(res.asFuture, 100.millis).getOrElse(fail("Observable resulted in None, Some expected"))
    })
  }

  it should "produce a timed observable of the integration of a simulation" in {
    check(Prop.forAll(diffIntervalBoundGen, diffFnGen, diffInitialGen) { (from, fn, initial) =>
      val (desc, der, int) = fn
      val dt = 10.millis
      val resFuture = rk4
        .integral(der, initial)
        .delayedObservable(from, dt)
        .take(100.millis)
        .foldLeft(List.empty[Result[Double, Double]])((l, r) => r :: l)
        .map(_.reverse)
        .asFuture

      val res = Await.result(resFuture, 150.millis).getOrElse(fail("Observable resulted in None, Some expected"))

      res.size should equal(10 +- 5)

      res
        .map(checkDiffResult(desc, from, dt.toMillis / 1000.0, initial, int))
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)
    })
  }

  it should "produce a sampled observable of the integration of a simulation" in {
    check(Prop.forAll(diffIntervalBoundGen, diffFnGen, diffInitialGen) { (from, fn, initial) =>
      val ticker = {
        lazy val t0 = System.currentTimeMillis()
        Observable.interval(20.millis).take(5).map(_ => (System.currentTimeMillis() - t0) / 1000.0)
      }
      val (desc, der, int) = fn
      val dt = 10.millis
      val resFuture = rk4
        .integral(der, initial)
        .sampledObservable(from, ticker, dt, 5)
        .take(100.millis)
        .foldLeft(List.empty[ObservedResult[Double, Double]])((l, r) => r :: l)
        .map(_.reverse)
        .asFuture

      val res = Await.result(resFuture, 150.millis).getOrElse(fail("Observable resulted in None, Some expected"))

      res.size should equal(5 +- 1)

      res
        .map(or => Result(or.value, or.at))
        .map(checkDiffResult(desc, from, dt.toMillis / 1000.0, initial, int))
        .foldLeft(Prop.forAll((_: Unit) => true))(_ && _)

      Prop.forAll((_: Unit) => true)
    })
  }
}
