package astrac.galileo

import astrac.galileo.rk4.auto._
import spire.std.double._
import org.scalatest.{FlatSpec, Matchers}

object KineticSpecs {

  type Vec = data.Vec2[Double]

  def vector(x: Double, y: Double) = data.vector2(x, y)

  val kinetic = data.kineticBuilder[Vec, Vec, Vec, Vec, Double, Double]

  type KineticFn = (kinetic.Particle, Double) => kinetic.Derivative

  def velocity(p: kinetic.Particle) = vector(p.momentum.x / p.mass, p.momentum.y / p.mass)

}

class KineticSpecs extends FlatSpec with Matchers {
  import KineticSpecs._

  val dt = 0.01

  "A particle in a kinetic system" should "keep its velocity if no force is applied" in {
    val b = kinetic.particle(vector(0.0, 0.0), vector(1.0, 1.0), 1)
    val f = vector(0, 0)
    val fn: KineticFn = (b, _) => kinetic.derivative(velocity(b), f, 0)

    val finalPoint = rk4.integral(fn, b).inInterval(0.0, 10.0, dt).value

    velocity(finalPoint).x should equal(1.0 +- 0.1)
    velocity(finalPoint).y should equal(1.0 +- 0.1)
    finalPoint.position.x should equal(10.0 +- 0.1)
    finalPoint.position.y should equal(10.0 +- 0.1)
  }

  it should "move by 5 metres and reach a velocity of 1 m/s if it has mass 10 Kg and is subject to 1 N of force for 10 seconds" in {
    val b = kinetic.particle(vector(0.0, 0.0), vector(0.0, 0.0), 10)
    val sqrt2 = math.sqrt(2)
    val f = vector(sqrt2, sqrt2) // Magnitude 1 force vector at 45 degrees
    val fn: KineticFn = (b, _) => kinetic.derivative(velocity(b), f, 0)

    val finalPoint = rk4.integral(fn, b).inInterval(0.0, 10.0, dt).value

    velocity(finalPoint).x should equal(sqrt2 +- 0.1)
    velocity(finalPoint).y should equal(sqrt2 +- 0.1)
    finalPoint.position.x should equal(sqrt2 * 5 +- 0.1)
    finalPoint.position.y should equal(sqrt2 * 5 +- 0.1)
  }

  it should "be at rest if a force is applied for in a direction and then an equal force is applied in the opposite one for the same amount of time" in {
    val vecSpace = implicitly[spire.algebra.VectorSpace[Vec, Double]]

    val b = kinetic.particle(vector(0.0, 0.0), vector(0.0, 0.0), 1)
    val f = vector(2, 3)
    val fn: KineticFn = (b, t) => kinetic.derivative(velocity(b), if (t < 5) f else vecSpace.negate(f), 0)

    val finalPoint = rk4.integral(fn, b).inInterval(0.0, 10.0, dt).value

    velocity(finalPoint).x should equal(0.0 +- 0.1)
    velocity(finalPoint).y should equal(0.0 +- 0.1)
  }
}
