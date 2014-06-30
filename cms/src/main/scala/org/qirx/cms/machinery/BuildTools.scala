package org.qirx.cms.machinery

import scala.language.implicitConversions
import scala.language.higherKinds

object BuildTools extends Branching {
 
  implicit def toProgram[F[_], A](fa:F[A]):Program[F, A] = Program(fa)
  
  implicit def toCombinedType[F[_], A, O[_]](p:Program[F, A])(
      implicit ev: F ~> O
  ):Program[O, A] = ???
}