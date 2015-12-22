package astrac.galileo.rk4

import astrac.galileo.Representable
import scala.annotation.tailrec
import spire.algebra.{Field, Order, VectorSpace}

trait Integrator {

  private implicit def iterableVectorSpace[V, S](
    implicit
    vs: VectorSpace[V, S]
  ) = new VectorSpace[Iterable[V], S] {
    def negate(x: Iterable[V]): Iterable[V] = x.map(vs.negate)

    def zero: Iterable[V] = Seq.empty

    def plus(xs: Iterable[V], ys: Iterable[V]): Iterable[V] = {
      val pairs =
        if (xs.size >= ys.size) xs.zip(ys ++ Iterable.fill(xs.size - ys.size)(vs.zero))
        else (xs ++ Iterable.fill(ys.size - xs.size)(vs.zero)).zip(ys)

      pairs.map((vs.plus _).tupled)
    }

    def timesl(r: S, v: Iterable[V]): Iterable[V] = v.map(vs.timesr(_, r))

    implicit def scalar: Field[S] = vs.scalar
  }

  def toGen[VS, S, VD, VRepr](fn: (VS, S) => VD)(
    implicit
    vsRepr: Representable.Aux[VS, VRepr],
    vdRepr: Representable.Aux[VD, VRepr]
  ): (VRepr, S) => VRepr = (vr, s) => vdRepr.to(fn(vsRepr.from(vr), s))

  def toIterable[VS, S, VD, VRepr](fn: (VS, S) => VD)(
    implicit
    vsRepr: Representable.Aux[VS, VRepr],
    vdRepr: Representable.Aux[VD, VRepr]
  ): (Iterable[VRepr], S) => Iterable[VRepr] =
    (vrs, s) => vrs.map(vr => vdRepr.to(fn(vsRepr.from(vr), s)))

  def integral[VS, VD, S, VRepr](fn: (VS, S) => VD, initial: VS)(
    implicit
    vsRepr: Representable.Aux[VS, VRepr],
    vdRepr: Representable.Aux[VD, VRepr],
    vs: VectorSpace[VRepr, S],
    ord: Order[S]
  ) = new Integral[VRepr, S, VS](toGen(fn), vsRepr.to(initial))

  def integral[VS, VD, S, VRepr](fn: (VS, S) => VD, initial: Iterable[VS])(
    implicit
    vsRepr: Representable.Aux[VS, VRepr],
    vdRepr: Representable.Aux[VD, VRepr],
    vstRepr: Representable.Aux[Iterable[VS], Iterable[VRepr]],
    vdtRepr: Representable.Aux[Iterable[VD], Iterable[VRepr]],
    vs: VectorSpace[VRepr, S],
    ord: Order[S]
  ) = new Integral[Iterable[VRepr], S, Iterable[VS]](toIterable(fn), initial.map(vsRepr.to))

  def integral[V, S, VRepr](fn: S => V)(
    implicit
    vs: VectorSpace[VRepr, S],
    ord: Order[S],
    vsRepr: Representable.Aux[V, VRepr]
  ) = new Integral[VRepr, S, V](toGen((_: V, s: S) => fn(s)), vs.zero)
}
