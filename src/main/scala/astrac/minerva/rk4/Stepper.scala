package astrac.minerva.rk4

class Stepper[S, T, D](fn: (S, T) => D)(implicit int: Integrable[S, D, T]) {
  def in(t: T, dt: T)(initial: S) = step(initial, fn, t, dt)

  def evaluate(
    state: S,
    derivate: (S, T) => D,
    t: T,
    dt: T,
    lastDerivate: D
  ): D = {
    derivate(
      int.state.semigroup.combine(state, int.state.fromDerivate(lastDerivate, dt)),
      int.time.group.combine(t, dt)
    )
  }

  def step(initial: S, fn: (S, T) => D, t: T, dt: T): S = {
    import int._

    val a = evaluate(initial, fn, t, time.group.empty, derivate.monoid.empty)
    val b = evaluate(initial, fn, t, time.half(dt), a)
    val c = evaluate(initial, fn, t, time.half(dt), b)
    val d = evaluate(initial, fn, t, dt, c)

    val dxdt = derivate.scale(derivate.monoid.combineAll(
      a ::
        derivate.scale(derivate.monoid.combine(b, c), 2) ::
        d ::
        Nil
    ), 1.0 / 6.0)

    state.semigroup.combine(initial, state.fromDerivate(dxdt, dt))
  }
}
