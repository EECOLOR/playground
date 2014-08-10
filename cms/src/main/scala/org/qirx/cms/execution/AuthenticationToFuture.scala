package org.qirx.cms.execution

import scala.concurrent.Future

import org.qirx.cms.construction.Authenticate
import org.qirx.cms.construction.Authentication
import org.qirx.cms.machinery.~>

import play.api.mvc.RequestHeader

class AuthenticationToFuture(authentication: RequestHeader => Future[Boolean])
  extends (Authentication ~> Future) {

  def transform[x] = {
    case Authenticate(request) => authentication(request)
  }
}