// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec

import scala.concurrent.duration._
import scala.util.Random
import scala.collection.mutable.ListBuffer

object FragmentSpec extends Specification {

  "DefaultFragment" - {

    "instantiation should" - {
      "not evaluate the code that is passed in" - {
        new DefaultFragment(null, ???, new MockEventBus)
        success
      }

      "set the correct title" - {
        val title = "title-" + Random.nextInt
        val section = new DefaultFragment(title, ???, new MockEventBus)
        section.title is title
      }

      "push an event to the event bus" - {
        val eventBus = new MockEventBus
        val section = new DefaultFragment("", ???, eventBus)
        eventBus.events is Seq(Fragment.Created(section))
      }

      "not respond to it's own created event" - {
        val eventBus = new MockEventBus
        val section = new DefaultFragment("", ???, eventBus)
        eventBus.subscriptions is Seq.empty
      }

    }

    "fragment body instances should" - {

      "have a todo constructor that returns a fragment body" -
        isBody(Fragment.Body.Todo)

      "have a success constructor that returns a fragment body" -
        isBody(Fragment.Body.Success)

      "have a pending constructor that returns a fragment body" -
        isBody(Fragment.Body.Pending(""))

      "have an implicit conversion for unit" - {
        val unit = ()
        isBody(unit)
      }

      "have an implicit conversion for section" - {
        val section = newFragment()
        isBody(section)
      }
    }

    "execution should" - {

      "return pending todo for unit" - {
        execute() is Pending(defaultTitle, "TODO")
      }

      "return pending todo for todo" - {
        val result = execute(Fragment.Body.Todo)
        result is Pending(defaultTitle, "TODO")
      }

      "return pending for pending" - {
        val result = execute(Fragment.Body.Pending("message"))
        result is Pending(defaultTitle, "message")
      }

      "return success for success" - {
        val result = execute(Fragment.Body.Success)
        result isLike {
          case Success(title) => title is defaultTitle
        }
      }

      "return unexpected failure for an exception" - {
        val e = new RuntimeException("unexpected")
        val result = execute(throw e)
        result is UnexpectedFailure(defaultTitle, e)
      }

      "measure duration" - {
        val result1 = execute(Fragment.Body.Success)
        val result2 = execute {
          Thread.sleep(101)
          Fragment.Body.Success
        }

        result1 isLike {
          case s: Success => s.duration isLessThan 100.millis
        }
        result2 isLike {
          case s: Success => s.duration isMoreThan 100.millis
        }
      }

      "capture failures" - {
        val message = "custom failure"
        val result = execute(throw Fragment.ThrowableFailure("custom failure"))
        result is Failure(defaultTitle, message)
      }

      "correctly handle nested sections" - {
        val eventBus = new DefaultEventBus

        def newFragment(title: String)(code: => Fragment.Body) =
          new DefaultFragment(title, code, eventBus)

        val fragment =
          newFragment("level 1") {
            newFragment("level 2 - todo") {
              //todo
            }
            newFragment("level 2 - pending") {
              Fragment.Body.Pending("pending")
            }
            newFragment("level 2 - nested") {
              newFragment("level 3 - failure") {
                throw Fragment.ThrowableFailure("failure")
              }
              newFragment("level 3 - success") {
                Fragment.Body.Success
              }
            }
          }

        val result = fragment.execute
        result is
          CompoundResult("level 1",
            Seq(
              Pending("level 2 - todo", "TODO"),
              Pending("level 2 - pending", "pending"),
              CompoundResult("level 2 - nested",
                Seq(
                  Failure("level 3 - failure", "failure"),
                  Success("level 3 - success")(0.millis)
                )
              )
            )
          )
      }
    }
  }

  val defaultTitle = "title"

  def newFragment(code: => Fragment.Body) =
    new DefaultFragment(defaultTitle, code, new DefaultEventBus)

  def isBody(body: Fragment.Body) = success

  def execute(code: => Fragment.Body) =
    newFragment(code).execute

  implicit def numeric[T <: Duration]: Numeric[T] =
    new Numeric[T] {
      def fromInt(x: Int): T = ???
      def minus(x: T, y: T): T = ???
      def negate(x: T): T = ???
      def plus(x: T, y: T): T = ???
      def times(x: T, y: T): T = ???
      def toDouble(x: T): Double = ???
      def toFloat(x: T): Float = ???
      def toInt(x: T): Int = ???
      def toLong(x: T): Long = ???

      def compare(x: T, y: T): Int = x.compare(y)
    }

  class MockEventBus extends EventBus {
    class Subscription extends super.Subscription {
      def cancel(): Unit = ()
      def enable(): Unit = ()
      def disable(): Unit = ()
    }

    val subscriptions = ListBuffer.empty[Subscription]

    def onEvent(handler: Fragment.Event => Unit): Subscription = {
      val subscription = new Subscription
      subscriptions += subscription
      subscription
    }

    val events = ListBuffer.empty[Fragment.Event]

    def push(event: Fragment.Event): Unit = events += event
  }
}