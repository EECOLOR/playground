package org.qirx.cms.machinery

import scala.language.higherKinds

/**
 * I = InstructionType
 * R = ResultType
 * N = NewResultType or NewInstructionType
 * C = ResultContainerType
 */
sealed trait Program[I[_], R] {

  def andThen[N](f: R => Program[I, N]): Program[I, N] =
    this match {
      case Result(result) => f(result)
      case SuspendedAndThen(instruction, g) =>
        SuspendedAndThen(instruction, g andThen (_ andThen f))
    }

  def transformWith[N](f: R => N): Program[I, N] =
    andThen(result => Result(f(result)))

  def changeInstructionTypeTo[N[_]](implicit transform: I ~> N): Program[N, R] =
    this match {
      case Result(result) => Result(result)
      case SuspendedAndThen(instruction, f) =>
        SuspendedAndThen(transform(instruction), f andThen (_.changeInstructionTypeTo))
    }

  def executeWith[C[_]: Factory: Chainer](runner: I ~> C): C[R] = {
    val factory = implicitly[Factory[C]]

    this match {
      case Result(result) =>
        factory.create(result)

      case SuspendedAndThen(instruction, f) =>
        val result = runner transform instruction
        val chainer = implicitly[Chainer[C]]
        chainer.chain(result, to = f andThen (_ executeWith runner))
    }
  }

  // for-comprehension support
  def flatMap[B] = andThen[B] _
  // for-comprehension support
  def map[B] = transformWith[B] _
  // patterm match support in for-comprehension
  def withFilter(f: R => Boolean): Program[I, R] = this
}

object Program {
  def apply[I[_], R](f: I[R]): Program[I, R] = SuspendedAndThen(f, Result(_: R))

  trait Of[I[_]] {
    type T[x] = Program[I, x]
  }
}

case class SuspendedAndThen[I[_], R, N](
  instruction: I[R],
  andThen: R => Program[I, N]) extends Program[I, N]

case class Result[I[_], R](result: R) extends Program[I, R]