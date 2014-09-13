package org.qirx.cms.api

import org.qirx.cms.metadata.DocumentMetadata
import play.api.mvc.Request
import play.api.mvc.AnyContent
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.System
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Branch
import org.qirx.cms.construction.Index.List
import org.qirx.cms.construction.Index.Get
import play.api.mvc.Result
import org.qirx.cms.construction.Index
import scala.language.higherKinds
import org.qirx.cms.construction.Return
import org.qirx.cms.construction.ValueOf
import org.qirx.cms.construction.api.GetFieldSetFromQueryString
import org.qirx.cms.construction.api.GetNextSegment
import play.api.libs.json.JsObject
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.construction.RemoveConfidentialProperties

class IndexRequestHandler[O[_]](
  meta: DocumentMetadata,
  request: Request[AnyContent],
  pathAtDocument: Seq[String])(
    implicit e: ProgramType[O],
    e1: System ~> O,
    e2: Index ~> O,
    e3: Metadata ~> O,
    e4: Branch[Result]#T ~> O) {

  import BuildTools._
  import Results._

  private val metaId = meta.id
  
  def get =
    for {
      fieldSet <- GetFieldSetFromQueryString(request.queryString)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone list(fieldSet)
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
      document <- Index.Get(metaId, id, fieldSet) ifNone Return(notFound)
    } yield ok(document)

  private def list(fieldSet: Set[String]) =
    for {
      documents <- Index.List(metaId, fieldSet)
    } yield ok(documents)
}