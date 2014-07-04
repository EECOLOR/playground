package org.qirx.cms

import org.qirx.cms.machinery.NaturalTransformation
import scala.language.higherKinds

package object execution {
  type Id[x] = x
  
  type ~>[-F[_], +G[_]] = NaturalTransformation[F, G]
}