package astrac.galileo.rk4

import shapeless.Generic
import spire.algebra.Field
import spire.algebra.VectorSpace

trait Integrable[FN, VS, VD, S, VRepr] {
  def sRepr: Integrable.Repr[VS, VRepr]
  def dRepr: Integrable.Repr[VD, VRepr]
  def gen(fn: FN): (VRepr, S) => VRepr
  def initial(fn: FN, s: S): VS
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

  implicit def simpleFromReprs[V, S, VRepr](
    implicit
    r: Repr[V, VRepr],
    vs: VectorSpace[VRepr, S]
  ) = new Integrable[S => V, V, V, S, VRepr] {
    implicit lazy val sRepr = r
    implicit lazy val dRepr = r
    def gen(fn: S => V): (VRepr, S) => VRepr = (_, s) => sRepr.to(fn(s))
    def initial(fn: S => V, s: S) = sRepr.from(vs.zero)
  }

  implicit def simpleFromGeneric[V, S, VRepr](
    implicit
    g: Generic.Aux[V, VRepr],
    vs: VectorSpace[VRepr, S]
  ) = new Integrable[S => V, V, V, S, VRepr] {
    implicit lazy val sRepr = Repr.fromGeneric(g)
    implicit lazy val dRepr = sRepr
    def gen(fn: S => V): (VRepr, S) => VRepr = (_, s) => sRepr.to(fn(s))
    def initial(fn: S => V, s: S) = sRepr.from(vs.zero)
  }

  type DiffEqWithInitial[VS, VD, S] = ((VS, S) => VD, VS)

  implicit def differentialFromReprs[VS, VD, S, VRepr](
    implicit
    sr: Repr[VS, VRepr],
    dr: Repr[VD, VRepr]
  ) = new Integrable[DiffEqWithInitial[VS, VD, S], VS, VD, S, VRepr] {
    implicit lazy val sRepr = sr
    implicit lazy val dRepr = dr
    def gen(fn: DiffEqWithInitial[VS, VD, S]): (VRepr, S) => VRepr = (r, s) => dRepr.to(fn._1(sRepr.from(r), s))
    def initial(fn: DiffEqWithInitial[VS, VD, S], s: S) = fn._2
  }

  implicit def differentialFromGeneric[VS, VD, S, VRepr](
    implicit
    sGen: Generic.Aux[VS, VRepr],
    dGen: Generic.Aux[VD, VRepr]
  ) = new Integrable[DiffEqWithInitial[VS, VD, S], VS, VD, S, VRepr] {
    implicit lazy val sRepr = Repr.fromGeneric(sGen)
    implicit lazy val dRepr = Repr.fromGeneric(dGen)
    def gen(fn: DiffEqWithInitial[VS, VD, S]): (VRepr, S) => VRepr = (r, s) => dRepr.to(fn._1(sRepr.from(r), s))
    def initial(fn: DiffEqWithInitial[VS, VD, S], s: S) = fn._2
  }
}
