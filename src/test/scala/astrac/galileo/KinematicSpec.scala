package astrac.galileo

import astrac.galileo.rk4.auto._
import cats.std.double._
import org.scalatest.{FlatSpec, Matchers}

object KinematicSpec {
  case class Vec(x: Double, y: Double)
  case class Particle(pos: Vec, vel: Vec)
  case class Derivative(vel: Vec, acc: Vec)
  type KinematicFn = (Particle, Double) => Derivative
}

class KinematicSpec extends FlatSpec with Matchers {
  import KinematicSpec._

  val minDt = 0.001
  val maxDt = 0.25

  "Integrating kinematics" should "correctly interpolate parabolic motion in 2D" in {
    // Constant downward acceleration of -1 m/s²
    val acc = Vec(0, -1.0)

    // Particle starts from (0, 0) with v(x) = 1 m/s and v(y) = 2 m/s
    val initial = Particle(Vec(0, 0.0001), Vec(1, 2))

    val fn: KinematicFn = (b, _) => Derivative(b.vel, acc)

    val ticks = Stream.from(0).map(_.toDouble / 100)

    val trajectory = ticks.zip(rk4.integrator(fn).compute(
      ticks,
      minDt,
      maxDt,
      initial
    ).takeWhile(_.pos.y > 0))

    trajectory should not be (empty)

    val highPoint = trajectory.maxBy(_._2.pos.y)
    highPoint._1 should equal(2.0 +- 0.01)
    highPoint._2.vel.x should equal(1.0 +- 0.01)
    highPoint._2.vel.y should equal(0.0 +- 0.01)
    highPoint._2.pos.x should equal(2.0 +- 0.01)
    highPoint._2.pos.y should equal(2.0 +- 0.01)

    val lastPoint = trajectory.last
    lastPoint._1 should equal(4.0 +- 0.01)
    lastPoint._2.vel.x should equal(1.0 +- 0.01)
    lastPoint._2.vel.y should equal(-2.0 +- 0.01)
    lastPoint._2.pos.x should equal(4.0 +- 0.01)
    lastPoint._2.pos.y should equal(0.0 +- 0.01)
  }
}
