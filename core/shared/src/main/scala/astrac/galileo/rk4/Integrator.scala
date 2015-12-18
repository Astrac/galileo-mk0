package astrac.galileo.rk4

import astrac.galileo.Representable
import scala.annotation.tailrec
import spire.algebra.{Order, VectorSpace}

trait Integrator {

  def toGen[VS, S, VD, VRepr](fn: (VS, S) => VD)(
    implicit
    vsRepr: Representable.Aux[VS, VRepr],
    vdRepr: Representable.Aux[VD, VRepr]
  ): (VRepr, S) => VRepr = (vr, s) => vdRepr.to(fn(vsRepr.from(vr), s))

  def integral[VS, VD, S, VRepr](fn: (VS, S) => VD, initial: VS)(
    implicit
    vsRepr: Representable.Aux[VS, VRepr],
    vdRepr: Representable.Aux[VD, VRepr],
    vs: VectorSpace[VRepr, S],
    ord: Order[S]
  ) = new Integral[VRepr, S, VS](toGen(fn), vsRepr.to(initial))

  def integral[V, S, VRepr](fn: S => V)(
    implicit
    vs: VectorSpace[VRepr, S],
    ord: Order[S],
    vsRepr: Representable.Aux[V, VRepr]
  ) = new Integral[VRepr, S, V](toGen((_: V, s: S) => fn(s)), vs.zero)

}
