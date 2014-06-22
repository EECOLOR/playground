package org.qirx.cms.system

import scala.concurrent.Future

trait System {
  def performAction[T](action:Action[T]):Future[T]
}