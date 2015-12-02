package astrac.galileo.rk4

import spire.algebra._
import spire.syntax.all._

trait Stepper {

  private def evaluate[V, S](
    fn: (V, S) => V
  )(
    state: V, t: S, dt: S, lastDerivative: V
  )(implicit vs: VectorSpace[V, S]): V = {
    import vs.scalar

    fn(state + (lastDerivative :* dt), t + dt)
  }

  def step[V, S](fn: (V, S) => V)(initial: V, t: S, dt: S)(implicit vs: VectorSpace[V, S]): V = {
    import vs.scalar

    val scalarTwo = scalar.fromInt(2)
    val scalarSix = scalar.fromInt(6)

    val a = evaluate(fn)(initial, t, scalar.zero, vs.zero)
    val b = evaluate(fn)(initial, t, dt / scalarTwo, a)
    val c = evaluate(fn)(initial, t, dt / scalarTwo, b)
    val d = evaluate(fn)(initial, t, dt, c)

    val dxdt = (a + b + b + c + c + d) :/ scalarSix

    initial + (dxdt :* dt)
  }
}
