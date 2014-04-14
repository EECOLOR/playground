package org.qirx.littlespec

import scala.collection.mutable.ListBuffer

trait FragmentContainer {

  private val _fragments = ListBuffer.empty[Fragment]
  val fragments: Seq[Fragment] = _fragments

  private val eventBus = new DefaultEventBus

  private val subscription =
    eventBus.onEvent {
      case Fragment.Created(section) => addSection(section)
    }

  private def addSection(section: Fragment): Unit = {
    _fragments +=
      new Fragment {
        def execute() = {
          // make sure child sections do not end up as toplevel ones
          subscription.disable()
          val result = section.execute()
          subscription.enable()
          result
        }
      }
  }

  implicit class FragmentConstructor(title: String) {

    def -[T](code: => T)(implicit asEnd: T => Fragment.Body): Fragment =
      new DefaultFragment(title, code, eventBus)
  }
}