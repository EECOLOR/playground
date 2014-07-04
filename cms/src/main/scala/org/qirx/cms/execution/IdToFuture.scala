package org.qirx.cms.execution

import scala.concurrent.Future

object IdToFuture extends (Id ~> Future) {

  def transform[x] = Future successful _
}