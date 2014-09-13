package org.qirx.cms.machinery

import scala.language.higherKinds

/**
 * In the functional world called Natural Transformation
 */
trait ContainerTransformation[-F[_], +G[_]] { fToG =>
  def transform[x]: F[x] => G[x]

  def apply[x](fx: F[x]): G[x] = transform[x](fx)

  def andThen[H[_]](gToH: G ~> H) =
    new (F ~> H) {
      def transform[x] = fToG.transform andThen gToH.transform
    }
}

object ContainerTransformation {

  implicit class ContainerTransformationEnhancements[F[_], G[_], X](fToG: X)(
    implicit xAsTransformation: X => F ~> G) {

    /* 
     * can not define this method directly on 
     * natural transformation because of
     * variance
     */ 
    def or[H[_], T[_]](hToT: H ~> T)(implicit hToG: H ~> T => H ~> G) =
      new (Co[H, F]#T ~> G) {
        def transform[x] =
          _.value match {
            case Left(head) => hToG(hToT) apply head
            case Right(tail) => fToG(tail)
          }
      }
  }
}
