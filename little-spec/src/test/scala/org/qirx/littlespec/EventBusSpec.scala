// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec

object EventBusSpec extends Specification {

  def newEventBus = new DefaultEventBus

  "DefaultEventBus should" - {

    "accept subscriptions" - {
      newEventBus.onEvent(identity)
      success
    }

    "handle events" - {
      newEventBus.push(testEvent)
      success
    }

    "push events to subscribers" - {
      var event1: Option[Fragment.Event] = None
      var event2: Option[Fragment.Event] = None

      val eventBus = newEventBus
      eventBus.onEvent(e => event1 = Some(e))
      eventBus.onEvent(e => event2 = Some(e))
      eventBus.push(testEvent)

      event1 is Some(testEvent)
      event2 is Some(testEvent)
    }

    "hand out subscriptions that" - {

      "cancel" - {
        var event: Option[Fragment.Event] = None

        val eventBus = newEventBus
        val subscription = eventBus.onEvent(e => event = Some(e))
        subscription.cancel()
        eventBus.push(testEvent)

        event is None
      }

      "disable" - {
        var event: Option[Fragment.Event] = None

        val eventBus = newEventBus
        val subscription = eventBus.onEvent(e => event = Some(e))
        subscription.disable()
        eventBus.push(testEvent)

        event is None
      }

      "enable" - {
        var event: Option[Fragment.Event] = None

        val eventBus = newEventBus
        val subscription = eventBus.onEvent(e => event = Some(e))
        subscription.disable()
        subscription.enable()
        eventBus.push(testEvent)

        event is Some(testEvent)
      }

      "throw an illegal state exception if" - {
        "enable is called after being cancelled" - {
          val subscription = newSubscription
          subscription.cancel()
          subscription.enable() must throwAn[IllegalStateException]
        }
        "disable is called after being cancelled" - {
          val subscription = newSubscription
          subscription.cancel()
          subscription.disable() must throwAn[IllegalStateException]
        }
        "enable is called when enabled" - {
          val subscription = newSubscription
          subscription.enable() must throwAn[IllegalStateException]
        }
        "disable is called when disabled" - {
          val subscription = newSubscription
          subscription.disable()
          subscription.disable() must throwAn[IllegalStateException]
        }
      }
    }
  }

  val testEvent = Fragment.Created(new Fragment { def execute() = ??? })

  def newSubscription = newEventBus.onEvent(identity)
}