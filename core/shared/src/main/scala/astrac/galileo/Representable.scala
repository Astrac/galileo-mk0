package astrac.galileo

import shapeless.Generic

trait Representable[T] {
  type Repr
  def to(s: T): Repr
  def from(r: Repr): T
}

trait LowPriorityRepresentable {
  implicit def identity[T] = new Representable[T] {
    type Repr = T
    def to(s: T) = s
    def from(r: T) = r
  }
}

object Representable extends LowPriorityRepresentable {
  type Aux[T, R] = Representable[T] { type Repr = R }

  implicit def fromGeneric[T](implicit gen: Generic[T]) = new Representable[T] {
    type Repr = gen.Repr
    def to(s: T) = gen.to(s)
    def from(r: Repr) = gen.from(r)
  }

  implicit def forIterable[T](implicit gen: Representable[T]): Representable[Iterable[T]] = new Representable[Iterable[T]] {
    type Repr = Iterable[gen.Repr]
    def to(ss: Iterable[T]) = ss.map(gen.to(_))
    def from(r: Iterable[gen.Repr]) = r.map(gen.from)
  }
}
