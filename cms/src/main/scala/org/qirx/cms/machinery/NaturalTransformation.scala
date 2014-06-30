package org.qirx.cms.machinery

import scala.language.higherKinds

trait NaturalTransformation[-F[_], +G[_]] {
  def apply[x](f: F[x]): G[x]
}
