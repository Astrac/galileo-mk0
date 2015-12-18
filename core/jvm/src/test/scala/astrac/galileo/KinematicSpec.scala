package astrac.galileo

import astrac.galileo.rk4.auto._
import org.scalatest.{FlatSpec, Matchers}
import spire.std.double._

object KinematicSpec {

  type Vec = data.Vec2[Double]

  def vector(x: Double, y: Double) = data.Vec2(x, y)

  val kinematic = data.kinematicBuilder[Vec, Vec, Vec]

  type Particle = data.KinematicParticle[Vec, Vec]

  type Derivative = data.KinematicDerivative[Vec, Vec]

  type KinematicFn = (Particle, Double) => Derivative
}

class KinematicSpec extends FlatSpec with Matchers {
  import KinematicSpec._

  "Integrating kinematics" should "correctly approximate parabolic motion in 2D" in {
    // Constant downward acceleration of -1 m/s²
    val acceleration = vector(0, -1.0)

    // Particle starts from (0, 0) with v(x) = 1 m/s and v(y) = 2 m/s
    val initial = kinematic.particle(vector(0, 0.0001), vector(1, 2))

    val fn: KinematicFn = (b, _) => kinematic.derivative(b.velocity, acceleration)

    val trajectory = rk4
      .integral(fn, initial)
      .iterator(0.0, 0.01)
      .takeWhile(_.value.position.y >= 0)
      .toList

    val highPoint = trajectory.maxBy(_.value.position.y)
    highPoint.at should equal(2.0 +- 0.05)
    highPoint.value.velocity.x should equal(1.0 +- 0.05)
    highPoint.value.velocity.y should equal(0.0 +- 0.05)
    highPoint.value.position.x should equal(2.0 +- 0.05)
    highPoint.value.position.y should equal(2.0 +- 0.05)

    val lastPoint = trajectory.last
    lastPoint.at should equal(4.0 +- 0.05)
    lastPoint.value.velocity.x should equal(1.0 +- 0.05)
    lastPoint.value.velocity.y should equal(-2.0 +- 0.05)
    lastPoint.value.position.x should equal(4.0 +- 0.05)
    lastPoint.value.position.y should equal(0.0 +- 0.05)
  }
}
