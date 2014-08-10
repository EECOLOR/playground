package org.qirx.cms.construction

import play.api.mvc.AnyContent
import play.api.mvc.Request

sealed trait Authentication[T]

case class Authenticate(request: Request[AnyContent]) extends Authentication[Boolean]