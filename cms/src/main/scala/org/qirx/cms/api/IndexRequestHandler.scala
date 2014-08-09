package org.qirx.cms.api

import org.qirx.cms.metadata.DocumentMetadata
import play.api.mvc.Request
import play.api.mvc.AnyContent
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.System
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Branch
import org.qirx.cms.construction.Index.List
import org.qirx.cms.construction.Index.Get
import play.api.mvc.Result
import org.qirx.cms.construction.Index
import scala.language.higherKinds
import org.qirx.cms.machinery.Free
import org.qirx.cms.construction.Return
import org.qirx.cms.construction.ValueOf
import org.qirx.cms.construction.api.GetFieldSetFromQueryString
import org.qirx.cms.construction.api.GetNextSegment
import play.api.libs.json.JsObject
import org.qirx.cms.machinery.BuildTools._
import org.qirx.cms.construction.api.DocumentsResult
import org.qirx.cms.construction.api.DocumentResult
import org.qirx.cms.construction.RemoveConfidentialProperties


class IndexRequestHandler[O[_]](meta: DocumentMetadata,
  request: Request[AnyContent],
  pathAtDocument: Seq[String])(
    implicit e: ProgramType[O],
    e1: System ~> O,
    e2: Index ~> O,
    e3: Store ~> O,
    e4: Metadata ~> O,
    e5: Branch[Result]#T ~> O) extends Results {

  def get =
    for {
      fieldSet <- GetFieldSetFromQueryString(request.queryString)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone list(fieldSet)
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
      actualId <- Store.GetActualId(meta.id, id)
      document <- Get(meta.id, actualId, fieldSet) ifNone Return(notFound)
      result <- DocumentResult(document)
    } yield result

  private def list(fieldSet: Set[String]) =
    for {
      documents <- List(meta.id, fieldSet)
      result <- DocumentsResult(documents)
    } yield result
}