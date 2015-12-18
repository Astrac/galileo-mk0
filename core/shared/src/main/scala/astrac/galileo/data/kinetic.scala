package astrac.galileo.data

case class KineticParticle[Position, Momentum, Mass](position: Position, momentum: Momentum, mass: Mass)

case class KineticDerivative[Velocity, Force, MassDerivative](velocity: Velocity, force: Force, massDerivative: MassDerivative)

class KineticBuilder[Position, Velocity, Momentum, Force, Mass, MassDerivative] private[data] {
  def particle(position: Position, momentum: Momentum, mass: Mass) = KineticParticle(position, momentum, mass)
  def derivative(velocity: Velocity, force: Force, massDerivative: MassDerivative) = KineticDerivative(velocity, force, massDerivative)
}
