package astrac.galileo

package object data {

  def vector2[Scalar](x: Scalar, y: Scalar) = Vec2(x, y)

  def kinematicBuilder[Position, Velocity, Acceleration, Time] =
    new KinematicBuilder[Position, Velocity, Acceleration, Time]()

  def kineticBuilder[Position, Velocity, Momentum, Force, Mass, MassDerivative, Scalar] =
    new KineticBuilder[Position, Velocity, Momentum, Force, Mass, MassDerivative, Scalar]()
}
