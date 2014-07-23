package org.qirx.cms

import scala.language.higherKinds
import scala.language.implicitConversions

package object machinery {

  type Id[x] = x

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]

  type Program[F[_], A] = Free[F, A]

  object Program {
    def apply[F[_], A, O[_]](fa: F[A])(
      implicit lift: F ~> O): Program[O, A] = Free(lift(fa))
  }

  trait ProgramType[T[_]]

  object ProgramType {
    implicit def anyType[T[_]]: ProgramType[T] = null
  }
}