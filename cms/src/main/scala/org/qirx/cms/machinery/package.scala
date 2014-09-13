package org.qirx.cms

import scala.language.higherKinds
import scala.language.implicitConversions

package object machinery {

  type Id[x] = x

  type ~>[-F[_], +G[_]] = ContainerTransformation[F, G]

  trait ProgramType[T[_]] {
    type Result[x] = T[x]
  }

  object ProgramType {
    implicit def anyType[T[_]]: ProgramType[T] = null
  }
}