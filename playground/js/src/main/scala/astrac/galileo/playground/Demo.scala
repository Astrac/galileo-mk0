package astrac.galileo.playground

import scalajs.js
import js.annotation.JSExport
import org.scalajs.dom

case class Vec2(x: Double, y: Double)

object Vec2 {
  def fromPolar(angle: Double, module: Double) = Vec2(math.cos(angle) * module, math.sin(angle) * module)
}

case class Particle(position: Vec2, velocity: Vec2)

case class Derivative(velocity: Vec2, acceleration: Vec2)

@JSExport("Demo")
object Demo extends js.JSApp {
  import scalatags.JsDom.all._

  val canvasWidth = 700
  val canvasHeight = 500

  def textField(_label: String, _id: String, _def: Option[String] = None) = {
    val in = input(
      `type` := "text",
      value := _def.getOrElse("")
    ).render

    (
      in,
      div(
        id := _id,
        label(
          `for` := _id,
          _label
        ),
        in
      )
    )
  }

  object Fields {
    val (gravityInput, gravityField) = textField("Gravity", "gravity", Some("100"))
    val (angleInput, angleField) = textField("Angle", "angle", Some("45"))
    val (velocityInput, velocityField) = textField("Velocity", "velocity", Some("100"))
  }

  val simCanvas = canvas(
    margin := "10 0",
    "width".attr := canvasWidth,
    "height".attr := canvasHeight
  ).render

  override def main(): Unit = {
    dom.document.body.appendChild(div(
      margin := 10,
      h1("Parabolic motion"),
      form(
        Fields.gravityField,
        Fields.angleField,
        Fields.velocityField,
        button(
          "Shoot!",
          onclick := { () =>
            shoot(
              Fields.gravityInput.value.toDouble,
              Fields.angleInput.value.toDouble,
              Fields.velocityInput.value.toDouble
            )
            false
          }
        )
      ),
      simCanvas
    ).render)

    ()
  }

  val graphics = simCanvas
    .getContext("2d")
    .asInstanceOf[dom.CanvasRenderingContext2D]

  def clear() = {
    graphics.clearRect(0, 0, simCanvas.width, simCanvas.height)
  }

  def redraw(particle: Particle): Unit = {
    clear()
    graphics.beginPath()
    graphics.lineWidth = 1.0
    graphics.arc(particle.position.x, particle.position.y, 5, 0, 2 * math.Pi)
    graphics.fillStyle = "green"
    graphics.fill()
    graphics.strokeStyle = "black"
    graphics.stroke()
    graphics.closePath()
  }

  def shoot(gravity: Double, angle: Double, velocity: Double) = {
    import astrac.galileo.rk4
    import rk4.auto._
    import spire.std.double._
    import scala.concurrent.duration._
    import monifu.concurrent.Implicits.globalScheduler

    val gravityFn: (Particle, Double) => Derivative = (p, t) => Derivative(p.velocity, Vec2(0, -gravity))

    rk4
      .integral(gravityFn, Particle(Vec2(0, 0), Vec2.fromPolar(angle.toRadians, velocity)))
      .delayedObservable(0.0, (1.0 / 60.0).seconds)
      .takeWhile(_.value.position.y >= 0)
      .foreach(p => redraw(p.value))
  }
}
