package org.qirx.cms.machinery

import scala.language.higherKinds

trait NaturalTransformation[-F[_], +G[_]] { self =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)

  def andThen[H[_]](toH: G ~> H) =
    new (F ~> H) {
      def transform[x] = self.transform andThen toH.transform
    }
}
