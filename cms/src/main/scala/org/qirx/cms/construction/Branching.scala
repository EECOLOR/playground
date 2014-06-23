package org.qirx.cms.construction

object Branching {

  import Lifting._

  def branch[A, B](left: Free[Action, A], right: Free[Action, B])(stayLeft: A => Boolean): Free[Action, A] =
    for {
      value1 <- left
      value2 <- right
      branched <- BranchAction(stayLeft(value1), left = value1, right = value2)
    } yield branched

  implicit class BooleanContinuation[F](action1: F)(
    implicit toBooleanAction: F => Free[Action, Boolean]) {

    def ifFalse[B](action2: Free[Action, B]): Free[Action, Boolean] =
      branch(action1, action2)(stayLeft = identity)

    def ifTrue[B](action2: Free[Action, B]): Free[Action, Boolean] =
      branch(action1, action2)(stayLeft = !_)
  }

  implicit class OptionContinuation[F, O, A](action1: F)(
    implicit asOptionAction: F => Free[Action, O],
    isOption: O => Option[A]) {

    def ifNone[B](action2: Free[Action, B]): Free[Action, A] =
      for {
        result <- branch(action1, action2)(stayLeft = _.isDefined)
      } yield result.get

    def ifSome[B](action2: Free[Action, B]): Free[Action, Option[A]] =
      for {
        result <- branch(action1, action2)(stayLeft = _.isEmpty)
      } yield result
  }

  implicit class IterableContinuation[F, I, A](action1: F)(
    implicit asIterableAction: F => Free[Action, I],
    isIterable: I => Iterable[A]) {

    def ifNonEmpty[B](action2: Free[Action, B]): Free[Action, I] =
      branch(action1, action2)(stayLeft = _.isEmpty)

    def ifEmpty[B](action2: Free[Action, B]): Free[Action, I] =
      branch(action1, action2)(stayLeft = _.nonEmpty)
  }
}