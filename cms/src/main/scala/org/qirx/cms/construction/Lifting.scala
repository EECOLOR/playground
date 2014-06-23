package org.qirx.cms.construction

import scala.language.higherKinds
import scala.language.implicitConversions

object Lifting {
  implicit def toFree[F[_], A](fa: F[A]): Free[F, A] =
    Bind(fa, (a: A) => Apply(a))
}