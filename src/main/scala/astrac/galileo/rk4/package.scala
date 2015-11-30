package astrac.galileo

import scala.annotation.tailrec

package object rk4 {

  def integrator[S, D, T](fn: T => D)(implicit int: Integrable[S, D, T]) = new Integrator((_: S, t: T) => fn(t))

  def integrator[S, D, T](fn: (S, T) => D)(implicit int: Integrable[S, D, T]) = new Integrator(fn)

  object auto extends AutoInstances

}
