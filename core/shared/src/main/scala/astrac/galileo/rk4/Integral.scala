package astrac.galileo.rk4

import astrac.galileo.Representable
import monifu.reactive.Observable
import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import spire.algebra.{Field, Order, VectorSpace}
import spire.syntax.order._
import spire.syntax.vectorSpace._

object Integral {

  private def evaluate[V, S](fn: (V, S) => V)(state: V, t: S, dt: S, lastDerivative: V)(
    implicit
    vs: VectorSpace[V, S],
    sc: Field[S]
  ): V = fn(state, t + dt)

  private def step[V, S](fn: (V, S) => V)(initial: V, from: S, dt: S)(
    implicit
    vs: VectorSpace[V, S]
  ): V = {
    import vs.scalar

    val scalarTwo = scalar.fromInt(2)
    val scalarSix = scalar.fromInt(6)

    val a = evaluate(fn)(initial, from, scalar.zero, vs.zero)
    val b = evaluate(fn)(initial, from, dt / scalarTwo, a)
    val c = evaluate(fn)(initial, from, dt / scalarTwo, b)
    val d = evaluate(fn)(initial, from, dt, c)

    val dxdt = (a + b + b + c + c + d) :/ scalarSix

    initial + (dxdt :* dt)
  }

  case class Result[V, S](value: V, at: S)
  case class ObservedResult[V, S](value: V, at: S, observedAt: S)
}

class Integral[V, S, Res](fn: (V, S) => V, initial: V)(
    implicit
    vs: VectorSpace[V, S],
    ord: Order[S],
    repr: Representable.Aux[Res, V]
) {
  import Integral._
  import vs.scalar

  private type ObservedResult = Integral.ObservedResult[Res, S]
  private type Result = Integral.Result[Res, S]

  def inInterval(from: S, to: S, dt: S): ObservedResult = {
    val (lower, upper, neg) = if (from < to) (from, to, false) else (to, from, true)

    def inIntervalRec(lower: S, initial: V): (V, S) = {
      if ((upper - lower) < dt) (initial, lower)
      else inIntervalRec(lower + dt, step(fn)(initial, lower, dt))
    }

    val normRes = inIntervalRec(lower, initial)
    val genRes = if (neg) (vs.negate(normRes._1), normRes._2) else normRes
    ObservedResult(repr.from(genRes._1), genRes._2, upper)
  }

  def iterator(from: S, dt: S): Iterator[Result] =
    Iterator.iterate((initial, from)) { last =>
      val nextT = last._2 + dt
      (step(fn)(last._1, nextT, dt), nextT)
    }.map(r => Result(repr.from(r._1), r._2))

  def stream(from: S, dt: S): Stream[Result] =
    iterator(from, dt).toStream

  def observable(from: S, dt: S): Observable[Result] =
    Observable.fromIterator(iterator(from, dt))

  def delayedObservable(from: S, delay: FiniteDuration): Observable[Result] = {
    val dt = scalar.fromDouble(delay.toMillis / 1000.0)
    Observable.interval(delay).zip(observable(from, dt)).map(_._2)
  }

  def sampledObservable(from: S, ticker: Observable[S], delay: FiniteDuration, maxSteps: Int): Observable[ObservedResult] = {
    val samples = ticker
      .combineLatest(delayedObservable(from, delay))
      .map(r => ObservedResult(r._2.value, r._2.at, r._1))

    samples.head.flatMap { first =>
      samples.scan((first, first))((l, c) => (l._2, c)).filter(e => e != first && e._1.observedAt != e._2.observedAt).map(_._2)
    }
  }
}
