package org.qirx.littlespec

import scala.concurrent.duration._
import scala.util.Try
import scala.collection.mutable.ListBuffer

trait Fragment {
  def execute(): Result
}

object Fragment {
  sealed trait Body

  case class ThrowableFailure(message: String) extends Throwable

  object Body {
    val Todo = Pending("TODO")
    case object Success extends Body
    case class Pending(message: String) extends Body

    import scala.language.implicitConversions
    implicit def unitToTodo(unit: Unit): Body = Todo
    implicit def fragmentToBody(fragment: Fragment): Body = Success
  }
}

class DefaultFragment(
  val title: String,
  code: => Fragment.Body,
  onSubFragmentCreated: (Fragment => Unit) => FragmentHandler.CancelSubscription) extends Fragment {

  def execute(): Result = {
    val (result, nestedResults, duration) = timedExecution()

    result
      .map {
        case Fragment.Body.Pending(message) =>
          Pending(title, message)
        case Fragment.Body.Success =>
          if (nestedResults.nonEmpty) CompoundResult(title, nestedResults)
          else Success(title)(duration)
      }
      .recover {
        case failure @ Fragment.ThrowableFailure(message) =>
          Failure(title, message, failure)
        case throwable: Throwable =>
          UnexpectedFailure(title, throwable)
      }.get
  }

  type TimedExecutionResults = (Try[Fragment.Body], Seq[Result], FiniteDuration)

  private def timedExecution(): TimedExecutionResults = {
    val startTime = System.nanoTime

    val (result, nestedResults) = executeCode()

    val endTime = System.nanoTime

    val duration = (endTime - startTime).nanoseconds

    (result, nestedResults, duration)
  }

  type ExecutionResults = (Try[Fragment.Body], Seq[Result])

  private def executeCode(): ExecutionResults = {
    val subfragments = ListBuffer.empty[Fragment]

    val cancelSubscription = onSubFragmentCreated(subfragments += _)
    val result = Try(code)
    cancelSubscription()

    val nestedResults = subfragments.map(_.execute())

    result -> nestedResults
  }

}
