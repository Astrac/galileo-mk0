package astrac.minerva

import scala.annotation.tailrec

package object rk4 {

  def stepper[S, D, T](fn: (S, T) => D)(implicit int: Integrable[S, D, T]) = new Stepper(fn)

  def integrator[S, D, T](fn: (S, T) => D)(implicit int: Integrable[S, D, T]) = new Integrator(fn)

}
