package astrac.galileo.data

case class KinematicParticle[Position, Velocity](position: Position, velocity: Velocity)

case class KinematicDerivative[Velocity, Acceleration](velocity: Velocity, acceleration: Acceleration)

class KinematicBuilder[Position, Velocity, Acceleration, Time] private[data] {
  type Particle = KinematicParticle[Position, Velocity]
  type Derivative = KinematicDerivative[Velocity, Acceleration]

  def particle(position: Position, velocity: Velocity) = KinematicParticle(position, velocity)
  def derivative(velocity: Velocity, acceleration: Acceleration) = KinematicDerivative(velocity, acceleration)

  type ParticleFn = (Particle, Time) => Derivative

  def constantAcceleration(acceleration: Acceleration): ParticleFn = (p, _) => derivative(p.velocity, acceleration)
}
