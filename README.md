# Galileo

[![Build status](https://travis-ci.org/Astrac/galileo.svg?branch=master)](https://travis-ci.org/Astrac/galileo)

## Intro

This project contains some experiments in creating a physics engine for scala/scala-js; I started it while reading and adapting [this series of articles](http://gafferongames.com/game-physics/integration-basics/) which use imperative programming and C++ for Scala and FP. At the moment it implements:

* A generic numeric integrator based on the [spire](https://github.com/non/spire) typeclasses
* Auto-derivation of type-classes via the [shapeless](https://github.com/milessabin/shapeless) product generalisation:
    * `VectorSpace[S, S]` is automatically derived for any `S` that has `Field[S]` instance
    * `VectorSpace[V, S]` is automatically derived for any `V` that is a product of types `Vx` for which `VectorSpace[Vx, S]` exists

## Integration

The integrator is accessible via the `astrac.galileo.rk4.integrate` function, which is defined as:

```scala
def integrate[FN, VS, VD, S, VRepr](
  fn: FN
)(
  samplingTimes: Iterable[S], minDt: S, maxDt: S, startState: Option[VS] = None
)(
  implicit int: Integrable[FN, VS, VD, S, VRepr], vs: VectorSpace[VRepr, S], sOrd: Order[S]
): Iterable[VS]
```
The arguments are:

* `fn` The function to integrate
* `samplingTimes` An iterable of times at which the integral must be calculated
* `minDt` The preferred rate of calculation (the smaller this is, the more precise the result is)
* `maxDt` The inverse of the minimum rate of calculation

### Generic integration

The generalisation is based on a shared generic representation of the _state_ and the _derivative_ types of the (differential) function being integrated. This generic representation must form a `VectorSpace` with the _scalar_ type, which in turn must be ordered via the `Order` typeclass.

The `Integrable` implicit is used to provide this generalisation and it is automatically provided for the following types:

* `(VS, S) => VD` - A differential function that given a state `VS` and a time `S` produces the derivative `VD`; available only if `VS` and `VD`:
* `S => VD` - A plain function of time `S`

Such an automatic instance of `Integrable` is available only if the vector-space parameters have one of:

* An `Integrable.Repr[V, VRepr]` implicitly available
* A shapeless product representation accessible via `Generic.Aux[V, VRepr]`

### Examples

```scala
import astrac.galileo.rk4
import spire.std.double._ // Bring in scope Field[Double] and Order[Double]
import rk4.auto._ // Enable auto-generation of needed instances

object SimpleIntegratoin {
  // Integrate f(x) = sin(x) between -π and π (result approx 0)
  println(rk4.integrate((x: Double) => math.sin(x))((-math.Pi to math.Pi by 0.01), 0.01, 0.01).last)
}

object KinematicIntegration {
  // Integrate kinematic equations
  case class Vec(x: Double, y: Double)
  case class Particle(pos: Vec, vel: Vec)
  case class Derivative(vel: Vec, acc: Vec)

  // Constant downward acceleration of -1 m/s²
  val acc = Vec(0, -1.0)

  // Particle starts from (0, 0) with v(x) = 1 m/s and v(y) = 2 m/s
  val initial = Particle(Vec(0, 0.0001), Vec(1, 2))

  val fn = (b: Particle, _: Double) => Derivative(b.vel, acc)

  // Simulate over a constant ticker
  val ticks = Stream.from(0).map(_.toDouble / 100)

  // Integrate until the particle "hits the ground"
  val trajectory = rk4.integrate(fn)(
    ticks,
    0.01,
    0.01,
    Some(initial)
  ).takeWhile(_.pos.y > 0)

  // The last point of the trajectory is 4 meters ahead and hits with a vertical speed of -2 m/s
  println(trajectory.last)
}
```

For more examples see [tests](https://github.com/Astrac/galileo/tree/master/src/test/scala/astrac/galileo).

## Further work

Things that I would like to do:

* Tidy up the `minDt` and `maxDt` definition, maybe provide overloaded functions that do not use them as they are important only when the `samplingTimes` depend on external factors
* Write some tests with scala-check
* Generalise concepts like kinematics, kinetics and so on
* Create a simulation engine that can use the generic concepts above
* Optimise for performance (e.g. use specialised types, reduce allocation of implicit objects)
* Create a scala-js playground showing the library's features
* Support other integration algorithms (e.g. euler, verlet)
