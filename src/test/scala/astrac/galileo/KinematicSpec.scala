package astrac.galileo

import astrac.galileo.rk4.auto._
import org.scalatest.{FlatSpec, Matchers}
import spire.std.double._

object KinematicSpec {
  case class Vec(x: Double, y: Double)
  case class Particle(pos: Vec, vel: Vec)
  case class Derivative(vel: Vec, acc: Vec)
  type KinematicFn = (Particle, Double) => Derivative
}

class KinematicSpec extends FlatSpec with Matchers {
  import KinematicSpec._

  "Integrating kinematics" should "correctly approximate parabolic motion in 2D" in {
    // Constant downward acceleration of -1 m/s²
    val acc = Vec(0, -1.0)

    // Particle starts from (0, 0) with v(x) = 1 m/s and v(y) = 2 m/s
    val initial = Particle(Vec(0, 0.0001), Vec(1, 2))

    val fn: KinematicFn = (b, _) => Derivative(b.vel, acc)

    val trajectory = rk4
      .integral(fn, initial)
      .iterator(0.0, 0.1)
      .takeWhile(_.value.pos.y >= 0)
      .toList

    val highPoint = trajectory.maxBy(_.value.pos.y)
    highPoint.at should equal(2.0 +- 0.01)
    highPoint.value.vel.x should equal(1.0 +- 0.01)
    highPoint.value.vel.y should equal(0.0 +- 0.01)
    highPoint.value.pos.x should equal(2.0 +- 0.01)
    highPoint.value.pos.y should equal(2.0 +- 0.01)

    val lastPoint = trajectory.last
    lastPoint.at should equal(4.0 +- 0.01)
    lastPoint.value.vel.x should equal(1.0 +- 0.01)
    lastPoint.value.vel.y should equal(-2.0 +- 0.01)
    lastPoint.value.pos.x should equal(4.0 +- 0.01)
    lastPoint.value.pos.y should equal(0.0 +- 0.01)
  }
}
