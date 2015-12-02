package astrac.galileo.rk4

import shapeless._
import spire.algebra.{Field, Order, VectorSpace}
import spire.syntax.all._

trait AutoInstances {
  implicit def trivialVectorSpace[T](implicit f: Field[T]) = new VectorSpace[T, T] {
    override def negate(x: T) = -x
    override def zero = f.zero
    override def plus(x: T, y: T) = x + y
    override def timesl(x: T, y: T) = x * y
    override def scalar = f
  }

  implicit def autoVectorSpaceHNil[T](implicit f: Field[T]) = new VectorSpace[HNil, T] {
    override def negate(x: HNil) = HNil
    override def zero = HNil
    override def plus(x: HNil, y: HNil) = HNil
    override def timesl(x: T, y: HNil) = HNil
    override def scalar = f
  }

  implicit def autoVectorSpaceHCons[H, L <: HList, T](
    implicit
    f: Field[T],
    hVecSp: Lazy[VectorSpace[H, T]],
    lVecSp: VectorSpace[L, T]
  ) = new VectorSpace[H :: L, T] {
    override def negate(x: H :: L) = hVecSp.value.negate(x.head) :: (-x.tail)
    override def zero = hVecSp.value.zero :: lVecSp.zero
    override def plus(x: H :: L, y: H :: L) = hVecSp.value.plus(x.head, y.head) :: (x.tail + y.tail)
    override def timesl(x: T, y: H :: L) = hVecSp.value.timesl(x, y.head) :: (x *: y.tail)
    override def scalar = f
  }

  implicit def autoVectorSpace[V, Repr, S](
    implicit
    f: Field[S],
    gen: Generic.Aux[V, Repr],
    genVecSp: VectorSpace[Repr, S]
  ) = new VectorSpace[V, S] {
    override def negate(x: V) = gen.from(genVecSp.negate(gen.to(x)))
    override def zero = gen.from(genVecSp.zero)
    override def plus(x: V, y: V) = gen.from(gen.to(x) + gen.to(y))
    override def timesl(x: S, y: V) = gen.from(x *: gen.to(y))
    override def scalar = f
  }
}
