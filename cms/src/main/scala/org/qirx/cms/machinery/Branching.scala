package org.qirx.cms.machinery

import scala.language.higherKinds
import org.qirx.cms.construction.Branched
import org.qirx.cms.construction.Branch

trait BranchEnhancements {

  private type Lift[F[_], B] = Branch[B]#Instance ~> F

  def branch[F[_], A, B, O[_]](p1: Program[F, A], p2: Program[F, B])(
    stayLeft: A => Boolean)(implicit l:Lift[F, B]): Program[F, A] = {
    val value: Program[F, Either[A, B]] =
      p1.flatMap {
        case a if stayLeft(a) => Apply(Left(a))
        case _ => p2 map Right.apply
      }
    value.flatMap(value => Program(Branch[B].Instance(value)))
  }

  implicit class BooleanContinuation[P, F[_]](program1: P)(
    implicit p: Parts[F],
    asBooleanProgram: P => Program[F, Boolean]) {

    def ifFalse[B](program2: Program[F, B])(implicit l:Lift[F, B]) =
      branch(program1, program2)(stayLeft = identity)

    def ifTrue[B](program2: Program[F, B])(implicit l:Lift[F, B]) =
      branch(program1, program2)(stayLeft = !_)
  }

  implicit class OptionContinuation[P, F[_], O, A](program1: P)(
    implicit p: Parts[F],
    asOptionProgram: P => Program[F, O],
    isOption: O => Option[A]) {

    def ifNone[B](program2: Program[F, B])(implicit l:Lift[F, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isDefined)
      } yield result.get

    def ifSome[B](program2: Program[F, B])(implicit l:Lift[F, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isEmpty)
      } yield result
  }

  implicit class IterableContinuation[P, F[_], I, A](program1: P)(
    implicit p: Parts[F],
    asIterableProgram: P => Program[F, I],
    isIterable: I => Iterable[A]) {

    def ifNonEmpty[B](program2: Program[F, B])(implicit l:Lift[F, B]) =
      branch(program1, program2)(stayLeft = _.isEmpty)

    def ifEmpty[B](program2: Program[F, B])(implicit l:Lift[F, B]) =
      branch(program1, program2)(stayLeft = _.nonEmpty)
  }
}