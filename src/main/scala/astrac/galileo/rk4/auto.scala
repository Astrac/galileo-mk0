package astrac.galileo.rk4

import cats.Group
import cats.Monoid
import shapeless._

trait AutoInstances {

  implicit lazy val autoGroupHNil = new Group[HNil] {
    lazy val empty = HNil
    def inverse(a: HNil) = HNil
    def combine(a: HNil, b: HNil) = HNil
  }

  implicit def autoGroupHCons[T, H <: HList](
    implicit
    tGroup: Lazy[Group[T]],
    hGroup: Lazy[Group[H]]
  ) = new Group[T :: H] {
    lazy val empty = tGroup.value.empty :: hGroup.value.empty
    def inverse(a: T :: H) = tGroup.value.inverse(a.head) :: hGroup.value.inverse(a.tail)
    def combine(a: T :: H, b: T :: H) = tGroup.value.combine(a.head, b.head) :: hGroup.value.combine(a.tail, b.tail)
  }

  implicit def autoGroup[T, Repr](
    implicit
    gen: Generic.Aux[T, Repr],
    genGroup: Group[Repr]
  ): Group[T] = new Group[T] {
    lazy val empty = gen.from(genGroup.empty)
    def inverse(a: T) = gen.from(genGroup.inverse(gen.to(a)))
    def combine(a: T, b: T) = gen.from(genGroup.combine(gen.to(a), gen.to(b)))
  }

  implicit def autoOrderingHNil: Ordering[HNil] = new Ordering[HNil] {
    def compare(x: HNil, y: HNil) = 0
  }

  implicit def autoOrderingHCons[H, T <: HList](
    implicit
    H: Ordering[H],
    T: Ordering[T]
  ): Ordering[H :: T] = new Ordering[H :: T] {
    def compare(x: H :: T, y: H :: T) = {
      val hcmp = H.compare(x.head, y.head)
      if (hcmp == 0) T.compare(x.tail, y.tail)
      else hcmp
    }
  }

  implicit def autoDerivativeHNil[T: Time] = new Derivative[HNil, T] {
    def monoid = autoGroupHNil
    lazy val time = implicitly[Time[T]]
    def scale(d: HNil, f: Double) = HNil
  }

  implicit def autoDerivativeHCons[T, H, L <: HList](
    implicit
    tm: Time[T],
    hDer: Lazy[Derivative[H, T]],
    lDer: Derivative[L, T],
    mon: Monoid[H :: L]
  ) = new Derivative[H :: L, T] {
    lazy val time = tm
    lazy val monoid = mon
    def scale(d: H :: L, f: Double) = hDer.value.scale(d.head, f) :: lDer.scale(d.tail, f)
  }

  implicit def autoDerivative[D, Repr, T](
    implicit
    tm: Time[T],
    gen: Generic.Aux[D, Repr],
    genDerivative: Derivative[Repr, T],
    mon: Monoid[D]
  ): Derivative[D, T] = new Derivative[D, T] {
    lazy val time = tm
    lazy val monoid = mon
    def scale(d: D, f: Double) = gen.from(genDerivative.scale(gen.to(d), f))
  }

  implicit def autoStateHNil[T](
    implicit
    tm: Time[T],
    dr: Derivative[HNil, T]
  ) = new State[HNil, HNil, T] {
    lazy val derivative = dr
    lazy val monoid = autoGroupHNil
    def fromDerivative(d: HNil, t: T) = HNil
    def scale(s: HNil, f: Double) = HNil
  }

  implicit def autoStateHCons[T, DH, DL <: HList, SH, SL <: HList](
    implicit
    tm: Time[T],
    hdr: Derivative[DH, T],
    ldr: Derivative[DL, T],
    dr: Derivative[DH :: DL, T],
    hst: State[SH, DH, T],
    lst: State[SL, DL, T],
    mon: Monoid[SH :: SL]
  ) = new State[SH :: SL, DH :: DL, T] {
    lazy val derivative = dr
    lazy val monoid = mon
    def fromDerivative(d: DH :: DL, t: T) =
      hst.fromDerivative(d.head, t) :: lst.fromDerivative(d.tail, t)
    def scale(s: SH :: SL, f: Double) =
      hst.scale(s.head, f) :: lst.scale(s.tail, f)
  }

  implicit def autoState[S, SRepr, D, DRepr, T](
    implicit
    dr: Derivative[D, T],
    sGen: Generic.Aux[S, SRepr],
    dGen: Generic.Aux[D, DRepr],
    genState: Lazy[State[SRepr, DRepr, T]],
    mon: Monoid[S]
  ): State[S, D, T] = new State[S, D, T] {
    lazy val derivative = dr
    lazy val monoid = mon
    def fromDerivative(d: D, t: T) = sGen.from(genState.value.fromDerivative(dGen.to(d), t))
    def scale(s: S, f: Double) = sGen.from(genState.value.scale(sGen.to(s), f))
  }
}
