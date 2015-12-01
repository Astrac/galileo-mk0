package astrac.galileo.rk4

class Stepper[S, T, D](fn: (S, T) => D)(implicit int: Integrable[S, D, T]) {
  def in(t: T, dt: T)(initial: S) = step(initial, t, dt)

  private def evaluate(
    state: S,
    t: T,
    dt: T,
    lastDerivative: D
  ): D = {
    fn(
      int.state.monoid.combine(state, int.state.fromDerivative(lastDerivative, dt)),
      int.time.group.combine(t, dt)
    )
  }

  private def step(initial: S, t: T, dt: T): S = {
    import int._

    val a = evaluate(initial, t, time.group.empty, derivative.monoid.empty)
    val b = evaluate(initial, t, time.half(dt), a)
    val c = evaluate(initial, t, time.half(dt), b)
    val d = evaluate(initial, t, dt, c)

    val dxdt = derivative.scale(derivative.monoid.combineAll(
      a ::
        derivative.scale(derivative.monoid.combine(b, c), 2) ::
        d ::
        Nil
    ), 1.0 / 6.0)

    state.monoid.combine(initial, state.fromDerivative(dxdt, dt))
  }
}
