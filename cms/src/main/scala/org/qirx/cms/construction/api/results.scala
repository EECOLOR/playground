package org.qirx.cms.construction.api

import org.qirx.cms.construction.DirectAction
import play.api.http.Status.UNPROCESSABLE_ENTITY
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import play.api.mvc.Result
import play.api.mvc.Results.UnprocessableEntity
import play.api.mvc.Results.Ok
import play.api.mvc.Results.Created
import play.api.libs.json.Writes

trait ResultCreation extends DirectAction[Result]

