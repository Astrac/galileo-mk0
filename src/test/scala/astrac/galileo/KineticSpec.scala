package astrac.galileo

import astrac.galileo.rk4.auto._
import cats.Group
import cats.std.double._
import org.scalatest.{FlatSpec, Matchers}

object KineticSpecs {
  case class Vec(x: Double, y: Double)

  object Vec {
    val group = implicitly[Group[Vec]]
  }

  case class Particle(position: Vec, momentum: Vec, mass: Double) {
    def vel = Vec(momentum.x / mass, momentum.y / mass)
  }

  case class Derivative(vel: Vec, force: Vec, massD: Double = 0)

  type KineticFn = (Particle, Double) => Derivative
}

class KineticSpecs extends FlatSpec with Matchers {
  import KineticSpecs._

  val minDt = 0.01
  val maxDt = 0.25
  val G = 1.0

  "A particle in a kinetic system" should "keep its velocity if no force is applied" in {
    val b = Particle(Vec(0.0, 0.0), Vec(1.0, 1.0), 1)
    val f = Vec(0, 0)
    val fn: KineticFn = (b, _) => Derivative(b.vel, f)

    val ticks = Stream.from(0).map(_.toDouble / 100).take(1000)
    val finalPoint: Particle = rk4.integrator(fn).compute(ticks, minDt, maxDt, b).last

    finalPoint.vel.x should equal(1.0 +- 0.1)
    finalPoint.vel.y should equal(1.0 +- 0.1)
    finalPoint.position.x should equal(10.0 +- 0.1)
    finalPoint.position.y should equal(10.0 +- 0.1)
  }

  it should "move by 5 metres and reach a velocity of 1 m/s if it has mass 10 Kg and is subject to 1 N of force for 10 seconds" in {
    val b = Particle(Vec(0.0, 0.0), Vec(0.0, 0.0), 10)
    val sqrt2 = math.sqrt(2)
    val f = Vec(sqrt2, sqrt2) // Magnitude 1 force vector at 45 degrees
    val fn: KineticFn = (b, _) => Derivative(b.vel, f)

    val ticks = Stream.from(0).map(_.toDouble / 100).take(1000)
    val finalPoint: Particle = rk4.integrator(fn).compute(ticks, minDt, maxDt, b).last

    finalPoint.vel.x should equal(sqrt2 +- 0.1)
    finalPoint.vel.y should equal(sqrt2 +- 0.1)
    finalPoint.position.x should equal(sqrt2 * 5 +- 0.1)
    finalPoint.position.y should equal(sqrt2 * 5 +- 0.1)
  }

  it should "be at rest if a force is applied for in a direction and then an equal force is applied in the opposite one for the same amount of time" in {
    val b = Particle(Vec(0.0, 0.0), Vec(0.0, 0.0), 1)
    val f = Vec(2, 3)
    val fn: KineticFn = (b, t) => Derivative(b.vel, if (t < 5) f else Vec.group.inverse(f))

    val ticks = Stream.from(0).map(_.toDouble / 100).take(1000)
    val finalPoint: Particle = rk4.integrator(fn).compute(ticks, minDt, maxDt, b).last

    finalPoint.vel.x should equal(0.0 +- 0.1)
    finalPoint.vel.y should equal(0.0 +- 0.1)
  }
}
