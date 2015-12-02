package astrac.galileo.rk4

import scala.annotation.tailrec
import shapeless.Generic
import spire.algebra._
import spire.syntax.all._

trait Integrator extends Stepper {

  case class Step[V, S](
    current: V,
    previous: Option[V],
    frameTime: S,
    symTime: S,
    accumulator: S
  )

  private def consume[V, S](fn: (V, S) => V)(
    initial: V, symTime: S, dt: S, accumulator: S
  )(
    implicit
    vs: VectorSpace[V, S], ord: Order[S]
  ): (V, Option[V], S, S) = {
    import vs.scalar

    @tailrec
    def consumeAcc(
      current: V, previous: Option[V], t: S, accumulator: S
    ): (V, Option[V], S, S) =
      if (accumulator < dt)
        (current, previous, t, accumulator)
      else
        consumeAcc(step(fn)(current, t, dt), Some(current), t + dt, accumulator - dt)

    consumeAcc(initial, None, symTime, accumulator)
  }

  private def interpolate[V, S](
    minDt: S
  )(
    step: Step[V, S]
  )(
    implicit
    vs: VectorSpace[V, S]
  ): V = {
    import vs.scalar

    step.previous.fold(step.current) { previous =>
      val alpha = step.accumulator / minDt
      (step.current :* alpha) + (previous :* (scalar.one - alpha))
    }
  }

  private def nextStep[V, S](
    fn: (V, S) => V
  )(
    minDt: S, maxDt: S
  )(
    lastStep: Step[V, S], newFrameTime: S
  )(
    implicit
    vs: VectorSpace[V, S],
    sOrd: Order[S]
  ): Step[V, S] = {
    import vs.scalar

    val accumulator = lastStep.accumulator + sOrd.min(maxDt, newFrameTime - lastStep.frameTime)

    val (newState, prevState, newSymTime, newAccumulator) =
      consume(fn)(lastStep.current, lastStep.symTime, minDt, accumulator)

    Step(newState, prevState orElse Some(lastStep.current), newFrameTime, newSymTime, newAccumulator)
  }

  def integrate[V, S](
    fn: (V, S) => V
  )(
    samplingTimes: Iterable[S],
    minDt: S,
    maxDt: S,
    startState: Option[V] = None
  )(
    implicit
    vs: VectorSpace[V, S],
    sOrd: Order[S]
  ): Iterable[V] =
    samplingTimes
      .headOption
      .fold(Iterable.empty[V]) { firstSample =>
        samplingTimes
          .tail
          .scanLeft(
            Step(startState.getOrElse(vs.zero), None, firstSample, firstSample, vs.scalar.zero)
          )(nextStep(fn)(minDt, maxDt))
          .map(interpolate(minDt))
      }

  def integrateGen[V, D, S, VRepr](
    fn: (V, S) => D
  )(
    samplingTimes: Iterable[S],
    minDt: S,
    maxDt: S,
    startState: Option[V] = None
  )(
    implicit
    sGen: Generic.Aux[V, VRepr],
    dGen: Generic.Aux[D, VRepr],
    vs: VectorSpace[VRepr, S],
    sOrd: Order[S]
  ): Iterable[V] = {
    val genFn: (VRepr, S) => VRepr = (vr, s) => dGen.to(fn(sGen.from(vr), s))

    integrate(genFn)(samplingTimes, minDt, maxDt, startState.map(sGen.to)).map(sGen.from)
  }
}
