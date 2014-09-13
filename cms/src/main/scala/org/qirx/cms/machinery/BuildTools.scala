package org.qirx.cms.machinery

import scala.language.implicitConversions
import scala.language.higherKinds
import org.qirx.cms.construction.{System => SystemInstruction}

trait BuildTools extends BranchEnhancements with Coproduct.Transformations {

  type TypeSet = {
    type T[_]
  }

  type System = TypeSet {
    type T[x] = SystemInstruction[x]
  }

  type +[Types <: TypeSet, Type[_]] = TypeSet {
    type T[x] = Co[Type, Types#T]#Product[x]
  }

  implicit def toProgram[F[_], A, O[_]](fa: F[A])(
    implicit p: ProgramType[O],
    lift: F ~> O): Program[O, A] = Program(lift(fa))

  implicit class ProgramEnhancements[F[_], A, O[_]](s: F[A]) {
    def asProgram(
      implicit p: ProgramType[O],
      lift: F ~> O): Program[O, A] = toProgram(s)
  }
}

object BuildTools extends BuildTools