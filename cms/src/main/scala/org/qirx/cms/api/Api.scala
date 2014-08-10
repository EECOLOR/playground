package org.qirx.cms.api

import scala.concurrent.Future

import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result

trait Api {
  def handleRequest(remainingPathSegments: Seq[String], request: Request[AnyContent]): Future[Result]
}