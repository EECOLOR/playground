package org.qirx.littlespec

import scala.concurrent.duration._
import scala.util.Try
import scala.collection.mutable.ListBuffer

trait Fragment {
  def execute():Result
}

object Fragment {
  sealed trait Body

  case class ThrowableFailure(message: String) extends Throwable

  sealed trait Event
  case class Created(section:Fragment) extends Event

  object Body {
    val Todo = Pending("TODO")
    case object Success extends Body
    case class Pending(message:String) extends Body

    import scala.language.implicitConversions
    implicit def unitToEnd(unit: Unit): Body = Todo
    implicit def sectionToEnd(section: Fragment): Body = Success
  }
}

class DefaultFragment(val title: String, code: => Fragment.Body, events: EventBus) extends Fragment {

  events.push(Fragment.Created(DefaultFragment.this))

  type ExecutionResults = (Try[Fragment.Body], Seq[Result])
  type TimedExecutionResults = (Try[Fragment.Body], Seq[Result], FiniteDuration)

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
        case Fragment.ThrowableFailure(message) => Failure(title, message)
        case t: Throwable => UnexpectedFailure(title, t)
      }.get
  }

  private def timedExecution(): TimedExecutionResults = {
    val startTime = System.nanoTime

    val (result, results) = executeCode()

    val endTime = System.nanoTime

    val duration = (endTime - startTime).nanoseconds

    (result, results, duration)
  }

  private def executeCode(): ExecutionResults = {
    val nestedResults = ListBuffer.empty[Result]

    var subscription: events.Subscription = null
    subscription =
      events.onEvent {
        case Fragment.Created(section) =>
          subscription.disable()
          nestedResults += section.execute()
          subscription.enable()
      }

    val result = Try(code)

    subscription.cancel()

    result -> nestedResults
  }

}
