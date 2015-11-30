package astrac.galileo.rk4

import scala.annotation.tailrec

class Integrator[S, T, D](fn: (S, T) => D)(implicit int: Integrable[S, D, T]) {
  lazy val fnStepper = new Stepper(fn)

  case class Step(
    current: S,
    previous: Option[S],
    frameTime: T,
    symTime: T,
    accumulator: T
  )

  private def consume(
    initial: S, symTime: T, dt: T, accumulator: T
  ): (S, Option[S], T, T) = {
    @tailrec
    def consumeAcc(
      current: S, previous: Option[S], t: T, accumulator: T
    ): (S, Option[S], T, T) =
      if (int.time.ordering.lt(accumulator, dt))
        (current, previous, t, accumulator)
      else consumeAcc(
        fnStepper.in(t, dt)(current),
        Some(current),
        int.time.group.combine(t, dt),
        int.time.group.remove(accumulator, dt)
      )

    consumeAcc(initial, None, symTime, accumulator)
  }

  private def interpolate(minDt: T)(step: Step): S = step.previous.fold(step.current) { previous =>
    val alpha = int.time.ratio(step.accumulator, minDt)

    int.state.monoid.combine(
      int.state.scale(step.current, alpha),
      int.state.scale(previous, 1 - alpha)
    )
  }

  private def nextStep(minDt: T, maxDt: T)(lastStep: Step, newFrameTime: T): Step = {

    val accumulator = int.time.group.combine(
      lastStep.accumulator,
      int.time.ordering.min(
        maxDt,
        int.time.group.combine(
          newFrameTime,
          int.time.group.inverse(lastStep.frameTime)
        )
      )
    )

    val (newState, prevState, newSymTime, newAccumulator) =
      consume(lastStep.current, lastStep.symTime, minDt, accumulator)

    Step(
      newState,
      prevState orElse Some(lastStep.current),
      newFrameTime,
      newSymTime,
      newAccumulator
    )
  }

  def compute(
    samplingTimes: Iterable[T],
    minDt: T,
    maxDt: T,
    startState: S = int.state.monoid.empty
  ): Iterable[S] =
    samplingTimes
      .headOption
      .fold(Iterable.empty[S]) { firstSample =>
        samplingTimes
          .tail
          .scanLeft(
            Step(startState, None, firstSample, firstSample, int.time.group.empty)
          )(nextStep(minDt, maxDt))
          .map(interpolate(minDt))
      }
}
