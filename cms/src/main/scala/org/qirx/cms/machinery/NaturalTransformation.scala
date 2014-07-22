package org.qirx.cms.machinery

import scala.language.higherKinds

trait NaturalTransformation[-F[_], +G[_]] { fToG =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)

  def andThen[H[_]](gToH: G ~> H) =
    new (F ~> H) {
      def transform[x] = fToG.transform andThen gToH.transform
    }
}
