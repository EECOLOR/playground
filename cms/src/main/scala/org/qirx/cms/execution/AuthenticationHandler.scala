package org.qirx.cms.execution

import org.qirx.cms.construction.Authentication
import scala.concurrent.Future
import org.qirx.cms.construction.Authenticate
import play.api.mvc.RequestHeader

class AuthenticationHandler(authentication: RequestHeader => Future[Boolean])
  extends (Authentication ~> Future) {

  def transform[x] = {
    case Authenticate(request) => authentication(request)
  }
}