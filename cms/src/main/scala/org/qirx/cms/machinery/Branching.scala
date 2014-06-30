package org.qirx.cms.machinery

import org.qirx.cms.construction.BranchAction
import scala.language.higherKinds
import org.qirx.cms.construction.Action
import org.qirx.cms.construction.BranchAction

trait Branching {

  def branch[F[_], G[_], A, B](p1:Program[F, A], p2:Program[G, B])(
      stayLeft: A => Boolean) = 
        Program(BranchAction(p1, p2, stayLeft))
  
  implicit class BooleanContinuation[P, F[_]](program1: P)(
    implicit asBooleanProgram: P => Program[F, Boolean]) {

    def ifFalse[G[_], B](program2: Program[G, B]) =
      branch(program1, program2)(stayLeft = identity)

   def ifTrue[G[_], B](program2: Program[G, B]) =
     branch(program1, program2)(stayLeft = !_)
  }
  implicit class OptionContinuation[P, F[_], O, A](program1: P)(
    implicit asOptionProgram: P => Program[F, O],
    isOption: O => Option[A]) {

    def ifNone[G[_], B](program2: Program[G, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isDefined)
      } yield result.get

    def ifSome[G[_], B](program2: Program[G, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isEmpty)
      } yield result
  }

  implicit class IterableContinuation[P, F[_], I, A](program1: P)(
    implicit asIterableProgram: P => Program[F, I],
    isIterable: I => Iterable[A]) {

    def ifNonEmpty[G[_], B](program2: Program[G, B]) =
      branch(program1, program2)(stayLeft = _.isEmpty)

    def ifEmpty[G[_], B](program2: Program[G, B]) =
      branch(program1, program2)(stayLeft = _.nonEmpty)
  }
}