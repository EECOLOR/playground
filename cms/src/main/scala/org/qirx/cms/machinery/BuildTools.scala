package org.qirx.cms.machinery

import scala.language.implicitConversions
import scala.language.higherKinds
import org.qirx.cms.construction

object BuildTools extends Branching {

  //  implicit def toTypeInferredProgram[F[_], A](fa:F[A]):TypeInferredProgram[F, A] = TypeInferredProgram(fa)

  type Base = TypeSet.Base
  type +[Types <: TypeSet, Type[_]] = TypeSet.+[Types, Type]

  implicit def toProgramWithRunner[Types <: TypeSet, F[_], A, O[_]](fa: F[A])(
    implicit runner: ProgramRunner[Types],
    result: TypeSet.ToType.Aux[Types, O],
    lift: F ~> O): Program[O, A] = Program(fa)

  implicit def toProgram[F[_], A, O[_]](fa: F[A])(
    implicit parts: Parts[O],
    lift: F ~> O): Program[O, A] = Program(fa)

}