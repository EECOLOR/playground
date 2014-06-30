package org.qirx.cms

import scala.language.higherKinds

package object machinery {

  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]

  type <~[+G[_], -F[_]] = NaturalTransformation[F, G]
  
}