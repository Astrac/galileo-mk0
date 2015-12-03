package astrac.galileo.rk4

import shapeless.Generic

trait Integrable[FN, VS, VD, S, VRepr] {
  def sRepr: Integrable.Repr[VS, VRepr]
  def dRepr: Integrable.Repr[VD, VRepr]
  def gen(fn: FN): (VRepr, S) => VRepr
}

object Integrable {
  trait Repr[T, R] {
    def to(s: T): R
    def from(r: R): T
  }

  object Repr {
    implicit def identity[T] = new Repr[T, T] {
      def to(s: T) = s
      def from(r: T) = r
    }

    def fromGeneric[T, R](gen: Generic.Aux[T, R]) = new Repr[T, R] {
      def to(s: T) = gen.to(s)
      def from(r: R) = gen.from(r)
    }
  }

  trait GenericDifferentialFunctionIntegrable[VS, VD, S, VRepr] extends Integrable[(VS, S) => VD, VS, VD, S, VRepr] {
    def gen(fn: (VS, S) => VD): (VRepr, S) => VRepr = (r, s) => dRepr.to(fn(sRepr.from(r), s))
  }

  implicit def simpleFromReprs[V, S, VRepr](implicit r: Repr[V, VRepr]) = new Integrable[S => V, V, V, S, VRepr] {
    implicit lazy val sRepr = r
    implicit lazy val dRepr = r
    def gen(fn: S => V): (VRepr, S) => VRepr = (_, s) => sRepr.to(fn(s))
  }

  implicit def simpleFromGeneric[V, S, VRepr](implicit g: Generic.Aux[V, VRepr]) = new Integrable[S => V, V, V, S, VRepr] {
    implicit lazy val sRepr = Repr.fromGeneric(g)
    implicit lazy val dRepr = sRepr
    def gen(fn: S => V): (VRepr, S) => VRepr = (_, s) => sRepr.to(fn(s))
  }

  implicit def differentialFromReprs[VS, VD, S, VRepr](implicit sr: Repr[VS, VRepr], dr: Repr[VD, VRepr]) =
    new GenericDifferentialFunctionIntegrable[VS, VD, S, VRepr] {
      implicit lazy val sRepr = sr
      implicit lazy val dRepr = dr
    }

  implicit def differentialFromGeneric[VS, VD, S, VRepr](
    implicit
    sGen: Generic.Aux[VS, VRepr],
    dGen: Generic.Aux[VD, VRepr]
  ) = new GenericDifferentialFunctionIntegrable[VS, VD, S, VRepr] {
    implicit lazy val sRepr = Repr.fromGeneric(sGen)
    implicit lazy val dRepr = Repr.fromGeneric(dGen)
  }
}
