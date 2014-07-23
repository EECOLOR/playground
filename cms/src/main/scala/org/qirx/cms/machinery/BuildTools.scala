package org.qirx.cms.machinery

import scala.language.implicitConversions
import scala.language.higherKinds
import org.qirx.cms.construction
import org.qirx.cms.construction.System

trait BuildTools extends BranchEnhancements with Coproduct.Transformations {

  type TypeSet = {
    type T[_]
  }

  type Base = TypeSet {
    type T[x] = System[x]
  }
  
  type +[Types <: TypeSet, Type[_]] = TypeSet {
    type T[x] = Co[Type, Types#T]#Product[x]
  }

  implicit def toProgram[F[_], A, O[_]](fa: F[A])(
    implicit p: ProgramType[O],
    lift: F ~> O): Program[O, A] = Program(fa)

}