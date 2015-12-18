package astrac.galileo

package object data {
  def vector2[Scalar](x: Scalar, y: Scalar) = Vec2(x, y)
  def kinematicBuilder[Position, Velocity, Acceleration] = new KinematicBuilder[Position, Velocity, Acceleration]()
  def kineticBuilder[Position, Velocity, Momentum, Force, Mass, MassDerivative] = new KineticBuilder[Position, Velocity, Momentum, Force, Mass, MassDerivative]()
}
