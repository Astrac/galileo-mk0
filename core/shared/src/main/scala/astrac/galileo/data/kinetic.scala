package astrac.galileo.data

import astrac.galileo.Representable
import spire.algebra.VectorSpace

case class KineticParticle[Position, Momentum, Mass, Velocity](
    position: Position,
    momentum: Momentum,
    mass: Mass
) {
  def velocity[GenRepr](
    implicit
    momRepr: Representable.Aux[Momentum, GenRepr],
    velRepr: Representable.Aux[Velocity, GenRepr],
    vs: VectorSpace[Momentum, Mass]
  ): Velocity = velRepr.from(momRepr.to(vs.divr(momentum, mass)))
}

case class KineticDerivative[Velocity, Force, MassDerivative](
  velocity: Velocity,
  force: Force,
  massDerivative: MassDerivative
)

class KineticBuilder[Position, Velocity, Momentum, Force, Mass, MassDerivative, Time] private[data] {
  type Particle = KineticParticle[Position, Momentum, Mass, Velocity]
  type Derivative = KineticDerivative[Velocity, Force, MassDerivative]

  def particle(position: Position, momentum: Momentum, mass: Mass): Particle = KineticParticle(position, momentum, mass)
  def derivative(velocity: Velocity, force: Force, massDerivative: MassDerivative): Derivative = KineticDerivative(velocity, force, massDerivative)

  type ParticleFn = (Particle, Time) => Derivative

  def constantForce[GenRepr](force: Force)(
    implicit
    momRepr: Representable.Aux[Momentum, GenRepr],
    velRepr: Representable.Aux[Velocity, GenRepr],
    mdm: spire.algebra.Field[MassDerivative], // TODO: Find a way to relax this, a monoid would be enough here
    vs: VectorSpace[Momentum, Mass]
  ): ParticleFn = (p, _) => KineticDerivative(p.velocity, force, mdm.zero)

  def timeFunction[GenRepr](fn: Time => Force)(
    implicit
    momRepr: Representable.Aux[Momentum, GenRepr],
    velRepr: Representable.Aux[Velocity, GenRepr],
    mdm: spire.algebra.Field[MassDerivative], // TODO: Find a way to relax this, a monoid would be enough here
    vs: VectorSpace[Momentum, Mass]
  ): ParticleFn = (p, t) => KineticDerivative(p.velocity, fn(t), mdm.zero)

  def stateFunction[GenRepr](fn: (Particle, Time) => Force)(
    implicit
    momRepr: Representable.Aux[Momentum, GenRepr],
    velRepr: Representable.Aux[Velocity, GenRepr],
    mdm: spire.algebra.Field[MassDerivative], // TODO: Find a way to relax this, a monoid would be enough here
    vs: VectorSpace[Momentum, Mass]
  ): ParticleFn = (p, t) => KineticDerivative(p.velocity, fn(p, t), mdm.zero)
}
