package org.qirx.cms.machinery

import scala.language.higherKinds
import org.qirx.cms.construction.Branch

trait BranchEnhancements {

  private type Lift[F[_], B] = Branch[B]#T ~> F

  def branch[F[_], A, B, O[_]](p1: Program[F, A], p2: => Program[F, B])(
    stayLeft: A => Boolean)(implicit lift: Lift[F, B]): Program[F, A] = {
    val value: Program[F, Either[A, B]] =
      p1.flatMap {
        case a if stayLeft(a) => Result(Left(a))
        case _ => p2 map Right.apply
      }
    value.flatMap(value => Program(lift(Branch[B].Instance(value))))
  }

  implicit class BooleanContinuation[P, F[_]](program1: P)(
    implicit p: ProgramType[F],
    asBooleanProgram: P => Program[F, Boolean]) {

    def ifFalse[B](program2: => Program[F, B])(implicit l: Lift[F, B]) =
      branch(program1, program2)(stayLeft = identity)

    def ifTrue[B](program2: => Program[F, B])(implicit l: Lift[F, B]) =
      branch(program1, program2)(stayLeft = !_)
  }

  implicit class OptionContinuation[P, F[_], O, A](program1: P)(
    implicit p: ProgramType[F],
    asOptionProgram: P => Program[F, O],
    isOption: O => Option[A]) {

    def ifNone[B](program2: => Program[F, B])(implicit l: Lift[F, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isDefined)
      } yield result.get

    def ifSome[B](program2: => Program[F, B])(implicit l: Lift[F, B]) =
      for {
        result <- branch(program1, program2)(stayLeft = _.isEmpty)
      } yield result
  }

  implicit class IterableContinuation[P, F[_], I, A](program1: P)(
    implicit p: ProgramType[F],
    asIterableProgram: P => Program[F, I],
    isIterable: I => Iterable[A]) {

    def ifNonEmpty[B](program2: => Program[F, B])(implicit l: Lift[F, B]) =
      branch(program1, program2)(stayLeft = _.isEmpty)

    def ifEmpty[B](program2: => Program[F, B])(implicit l: Lift[F, B]) =
      branch(program1, program2)(stayLeft = _.nonEmpty)
  }

  /**
   * It is now required that the branch is located at the head of the Coproduct.
   * Theoretically we could write a transformation that moves the branch to the 
   * front of the Coproduct or alternatively a way where it does not matter 
   * where the branch is located. For now it was too complicated and not a real
   * anoying problem.  
   */
  implicit class CoproductWithBranchEnhancements[F[_], A, In[_], Out[_]](
    program: Program[In, A])(implicit withBranchAtHead: In ~> Co[Branch[A]#T, Out]#T) {

    def mergeBranch: Program[Out, A] =
      program match {
        case Result(a) => Result(a)
        case x @ SuspendedAndThen(fa, f) =>
          withBranchAtHead(fa).value match {
            case Left(branch) =>
              branch.value match {
                case Left(any) => f(any).mergeBranch
                case Right(t) => Result(t)
              }
            case Right(withoutBranch) =>
              Program(withoutBranch).flatMap(f andThen (_.mergeBranch))
          }
      }
  }
}