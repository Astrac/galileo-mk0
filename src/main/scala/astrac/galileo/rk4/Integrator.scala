package astrac.galileo.rk4

import scala.annotation.tailrec
import shapeless.Generic
import spire.algebra._
import spire.math.Interval
import spire.syntax.all._

trait Integrator extends Stepper {

  case class Step[V, S](
    current: V,
    previous: Option[V],
    frameTime: S,
    symTime: S,
    accumulator: S
  )

  private def consume[V, S](fn: (V, S) => V)(initial: V, symTime: S, dt: S, accumulator: S)(
    implicit
    vs: VectorSpace[V, S], ord: Order[S]
  ): (V, Option[V], S, S) = {
    import vs.scalar

    @tailrec
    def consumeAcc(current: V, previous: Option[V], t: S, accumulator: S): (V, Option[V], S, S) =
      if (accumulator < dt)
        (current, previous, t, accumulator)
      else
        consumeAcc(step(fn)(current, t, dt), Some(current), t + dt, accumulator - dt)

    consumeAcc(initial, None, symTime, accumulator)
  }

  private def interpolate[V, S](minDt: S)(step: Step[V, S])(implicit vs: VectorSpace[V, S]): V = {
    import vs.scalar

    step.previous.fold(step.current) { previous =>
      val alpha = step.accumulator / minDt
      (step.current :* alpha) + (previous :* (scalar.one - alpha))
    }
  }

  private def nextStep[V, S](fn: (V, S) => V)(minDt: S, maxDt: S)(lastStep: Step[V, S], newFrameTime: S)(
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

  def integrateInterval[FN, VS, VD, S, VRepr](fn: FN)(interval: Interval[S], dt: S)(
    implicit
    int: Integrable[FN, VS, VD, S, VRepr],
    vs: VectorSpace[VRepr, S],
    sOrd: Order[S]
  ): VS = {
    import vs.scalar

    interval match {
      case spire.math.Bounded(l, _, _) =>
        int.sRepr.from(
          Stream
          .iterate(Step(int.sRepr.to(int.initial(fn, l)), None, l, l, vs.scalar.zero)) { last =>
            nextStep(int.gen(fn))(dt, dt)(last, last.frameTime + dt)
          }
          .takeWhile(interval ∋ _.symTime)
          .map(interpolate(dt))
          .last
        )
      case spire.math.Point(l) =>
        int.initial(fn, l)
      case _ =>
        throw new IllegalArgumentException(s"It is only possible to integrate over a bounded interval, $interval provided")
    }
  }

  // TODO: Use the above implementation also when integrating over a stream (or viceversa)
  def integrateStream[FN, VS, VD, S, VRepr](fn: FN)(samplingTimes: Iterable[S], minDt: S, maxDt: S)(
    implicit
    int: Integrable[FN, VS, VD, S, VRepr],
    vs: VectorSpace[VRepr, S],
    sOrd: Order[S]
  ): Iterable[VS] =
    samplingTimes
      .headOption
      .fold(Iterable.empty[VRepr]) { firstSample =>
        samplingTimes
          .tail
          .scanLeft(
            Step(int.sRepr.to(int.initial(fn, firstSample)), None, firstSample, firstSample, vs.scalar.zero)
          )(nextStep(int.gen(fn))(minDt, maxDt))
          .map(interpolate(minDt))
      }
      .map(int.sRepr.from)
}
