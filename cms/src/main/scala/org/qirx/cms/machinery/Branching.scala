package org.qirx.cms.machinery

import org.qirx.cms.construction.BranchAction
import scala.language.higherKinds
import org.qirx.cms.construction.Action
import org.qirx.cms.construction.BranchAction
import org.qirx.cms.construction.Structure

trait Branching {

  private type LiftTo[F[_]] = Structure ~> F

  def branch[F[_]: AvailableParts: LiftTo, A, B, O[_]](p1: Program[F, A], p2: Program[F, B])(
    stayLeft: A => Boolean): Program[F, A] =
    Program(BranchAction(p1, p2, stayLeft))

  implicit class BooleanContinuation[P, F[_]: AvailableParts: LiftTo](program1: P)(
    implicit asBooleanProgram: P => Program[F, Boolean]) {

    def ifFalse[B](program2: Program[F, B]) =
      branch(program1, program2)(stayLeft = identity)

    def ifTrue[B](program2: Program[F, B]) =
      branch(program1, program2)(stayLeft = !_)
  }

  implicit class OptionContinuation[P, F[_]: AvailableParts: LiftTo, O, A](program1: P)(
    implicit asOptionProgram: P => Program[F, O],
    isOption: O => Option[A]) {

    def ifNone[B](program2: Program[F, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isDefined)
      } yield result.get

    def ifSome[B](program2: Program[F, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isEmpty)
      } yield result
  }

  implicit class IterableContinuation[P, F[_]: AvailableParts: LiftTo, I, A](program1: P)(
    implicit asIterableProgram: P => Program[F, I],
    isIterable: I => Iterable[A]) {

    def ifNonEmpty[B](program2: Program[F, B]) =
      branch(program1, program2)(stayLeft = _.isEmpty)

    def ifEmpty[B](program2: Program[F, B]) =
      branch(program1, program2)(stayLeft = _.nonEmpty)
  }
}