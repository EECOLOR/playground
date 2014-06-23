package org.qirx.cms.api

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result

object NoApi extends Api {
  def handleRequest(remainingPath: Seq[String], request: Request[AnyContent]): Future[Result] =
    ???
}