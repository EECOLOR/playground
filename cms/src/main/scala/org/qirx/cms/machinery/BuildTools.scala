package org.qirx.cms.machinery

import scala.language.implicitConversions
import scala.language.higherKinds
import org.qirx.cms.construction
import org.qirx.cms.construction.System
import org.qirx.cms.construction.Branching

object BuildTools extends BranchEnhancements {

  //  implicit def toTypeInferredProgram[F[_], A](fa:F[A]):TypeInferredProgram[F, A] = TypeInferredProgram(fa)

  type Base = TypeSet {
    type Out[x]= Co[Branching, System]#Product[x]
  }
  type +[Types <: TypeSet, Type[_]] = TypeSet.+[Types, Type]

  implicit def toProgram[F[_], A, O[_]](fa: F[A])(
    implicit parts: Parts[O],
    lift: F ~> O): Program[O, A] = Program(fa)

}