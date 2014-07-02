package org.qirx.cms.construction.api

import play.api.libs.json.JsObject
import org.qirx.cms.construction.DirectAction
import play.api.mvc.Result

trait ResultCreation extends DirectAction[Result]
case class ValitationResultsToResult(validationResults: Seq[JsObject]) extends ResultCreation
case class DocumentsResult(documents: Seq[JsObject]) extends ResultCreation
case class DocumentResult(document: JsObject) extends ResultCreation
case class DocumentCreatedResult(id: String) extends ResultCreation
