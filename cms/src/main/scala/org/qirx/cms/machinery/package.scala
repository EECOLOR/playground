package org.qirx.cms

import scala.language.higherKinds
import scala.language.implicitConversions

package object machinery {

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]

  type Program[F[_], A] = Free[F, A]

  trait AvailableParts[Elem[_]]
      
  object Program {
    def apply[F[_], A, O[_]](fa: F[A])(
      implicit parts: AvailableParts[O],
      lift: F ~> O): Program[O, A] = Free(lift(fa))
  }
}