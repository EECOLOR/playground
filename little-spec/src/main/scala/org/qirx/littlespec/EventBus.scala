package org.qirx.littlespec

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet

trait EventBus {

  trait Subscription {
    def cancel(): Unit
    def enable(): Unit
    def disable(): Unit
  }

  def onEvent(handler: Fragment.Event => Unit): Subscription

  def push(event: Fragment.Event): Unit
}

class DefaultEventBus extends EventBus {

  private val subscriptions = HashSet.empty[Subscription]

  class Subscription(handler: Fragment.Event => Unit) extends super.Subscription {

    subscribe()
    private var enabled = true

    def handle(e: Fragment.Event) =
      if (enabled) handler(e)

    def cancel(): Unit = unsubscribe()

    def enable(): Unit =
      if (isTerminated) problem("This subscription has been cancelled")
      else if (enabled) problem("This subscription is already enabled")
      else enabled = true

    def disable(): Unit =
      if (isTerminated) problem("This subscription has been cancelled")
      else if (!enabled) problem("This subscription is already disabled")
      else enabled = false

    private def subscribe() = subscriptions += this
    private def unsubscribe() = subscriptions -= this
    private def isTerminated = !subscriptions.contains(this)

    private def problem(message: String) =
    throw new IllegalStateException(message)
  }

  def onEvent(handler: Fragment.Event => Unit): Subscription =
    new Subscription(handler)

  def push(event: Fragment.Event): Unit =
    subscriptions.foreach(_ handle event)

}
