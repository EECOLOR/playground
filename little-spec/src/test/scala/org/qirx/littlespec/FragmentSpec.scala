// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec

import scala.concurrent.duration._
import scala.util.Random
import scala.collection.mutable.ListBuffer

object FragmentSpec extends Specification {

  "DefaultFragment" - {

    "instantiation should" - {
      def fakeOnFragmentCreated(f: Fragment => Unit): FragmentHandler.CancelSubscription = ???

      "not evaluate the code that is passed in" - {
        new DefaultFragment(null, ???, fakeOnFragmentCreated)
        success
      }

      "set the correct title" - {
        val title = "title-" + Random.nextInt
        val fragment = new DefaultFragment(title, ???, fakeOnFragmentCreated)
        fragment.title is title
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

      "have an implicit conversion for fragment" - {
        val fragment = newFragment()
        isBody(fragment)
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

      "correctly handle nested fragments" - {
        val eventBus = new FragmentHandler

        def newFragment(title: String)(code: => Fragment.Body) = {
          val fragment = new DefaultFragment(title, code, eventBus.onFragmentCreated)
          eventBus.fragmentCreated(fragment)
          fragment
        }

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
    new DefaultFragment(defaultTitle, code, new FragmentHandler().onFragmentCreated)

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
}