package org.qirx.cms.machinery

import scala.language.higherKinds

/**
 * This is like an `Either`, but specialized for container types. 
 * `Either[Option[String], List[String]` is a valid representation for `Coproduct`,
 * it's required that both containers are about `String`. The following 
 * would be an invalid representation: `Either[Option[Int], List[String]]`.
 * 
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