package org.qirx.cms.construction

import play.api.mvc.Request
import play.api.mvc.AnyContent

sealed trait Authentication[T]

case class Authenticate(request: Request[AnyContent]) extends Authentication[Boolean]