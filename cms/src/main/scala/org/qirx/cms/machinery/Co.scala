// in a small package to make errors more readable
package qirx

import scala.language.higherKinds
import scala.annotation.implicitNotFound
import org.qirx.cms.machinery.Coproduct

/*
 * When we define this as Coproduct[F[_], G[_], x] implicit 
 * resolution falters in combination with partial application
 * like this: ({type T[x] = Coproduct[F, G, x]})#T 
 * 
 * For nested Coproducts, the X[_] will not match the partially applied
 * Coproduct.   
 */
class Co[Head[_], Tail[_]] {

  type T[x] = Product[x]

  case class Product[x](value: Either[Head[x], Tail[x]])(
    implicit ev: Coproduct.NotAtLeft[Head])

}

object Co {
  def apply[Head[_], Tail[_]] = new Co[Head, Tail]
}