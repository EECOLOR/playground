package org.qirx.cms.api

import scala.language.higherKinds
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Return
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Store.Delete
import org.qirx.cms.construction.Store.Get
import org.qirx.cms.construction.Store.List
import org.qirx.cms.construction.Store.Save
import org.qirx.cms.construction.Store.SaveIdReference
import org.qirx.cms.construction.System
import org.qirx.cms.construction.Validate
import org.qirx.cms.construction.api.AddId
import org.qirx.cms.construction.api.DocumentCreatedResult
import org.qirx.cms.construction.api.DocumentResult
import org.qirx.cms.construction.api.DocumentsResult
import org.qirx.cms.construction.api.ExtractId
import org.qirx.cms.construction.api.GetFieldSetFromQueryString
import org.qirx.cms.construction.api.GetNextSegment
import org.qirx.cms.construction.api.Merge
import org.qirx.cms.construction.api.ToJsObject
import org.qirx.cms.construction.api.ToJsValue
import org.qirx.cms.construction.api.ValitationResultsToResult
import org.qirx.cms.machinery.BuildTools.IterableContinuation
import org.qirx.cms.machinery.BuildTools.OptionContinuation
import org.qirx.cms.machinery.BuildTools.toProgram
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.{~> => ~>}
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsObject
import play.api.mvc.AnyContent
import play.api.mvc.Request
import org.qirx.cms.construction.Branch
import play.api.mvc.Result
import org.qirx.cms.construction.Index

/**
 * The implicit parameters are used to make sure we have the correct
 * program type. The ProgramType[O] is supplied and we make sure that
 * all of our elements are present within the program type (there is a
 * natural transformation from the part to the program type).
 *
 * This construction allows for a different program type containing more
 * languages than needed in this class. That allows for the programs to be
 * used in other parts of the application. For an example usage of this class
 * check out the PrivateApi class.
 */
class DocumentRequestHandler[O[_]](
  meta: DocumentMetadata,
  request: Request[AnyContent],
  pathAtDocument: Seq[String])(
    implicit e: ProgramType[O],
    e1: System ~> O,
    e2: Store ~> O,
    e3: Index ~> O,
    e4: Metadata ~> O,
    e5: Branch[Result]#T ~> O) extends Results {

  def get =
    for {
      fieldSet <- GetFieldSetFromQueryString(request.queryString)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone list(fieldSet)
      _ <- Return(pathAfterId) ifNonEmpty Return(notFound)
      document <- Get(meta.id, id, fieldSet) ifNone Return(notFound)
      result <- DocumentResult(document)
    } yield result

  private def list(fieldSet: Set[String]) =
    for {
      documents <- List(meta.id, fieldSet)
      result <- DocumentsResult(documents)
    } yield result

  def post =
    for {
      _ <- GetNextSegment(pathAtDocument) ifSome Return(notFound)
      json <- ToJsValue(request) ifNone Return(badRequest)
      document <- ToJsObject(json) ifNone Return(jsonExpected)
      messages <- GetMessages(meta)
      results <- Validate(meta, document, Set.empty, messages) ifEmpty
        create(document)
      result <- ValitationResultsToResult(results)
    } yield result

  private def create(document: JsObject) = {
    val id = meta.idGenerator.generateFor(document)
    for {
      documentWithId <- AddId(document, id)
      _ <- Save(meta.id, id, documentWithId)
      _ <- Index.Put(meta.id, id, documentWithId)
      result <- DocumentCreatedResult(id)
    } yield result
  }

  def put =
    for {
      json <- ToJsValue(request) ifNone Return(badRequest)
      newDocument <- ToJsObject(json) ifNone Return(jsonExpected)
      messages <- GetMessages(meta)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone Return(notFound)
      _ <- Return(pathAfterId) ifNonEmpty Return(notFound)
      oldDocument <- Get(meta.id, id, Set.empty) ifNone Return(notFound)
      fieldSet <- GetFieldSetFromQueryString(request.queryString)
      results <- Validate(meta, newDocument, fieldSet, messages) ifEmpty
        update(id, oldDocument, newDocument, fieldSet)
      result <- ValitationResultsToResult(results)
    } yield result

  private def update(id: String, oldDocument: JsObject, newDocument: JsObject, fieldSet: Set[String]) =
    for {
      merged <- Merge(oldDocument, newDocument, fieldSet)
      newId <- ExtractId(newDocument)
      actualId = newId.getOrElse(id)
      documentWithId <- AddId(merged, actualId)
      _ <- SaveIdReference(meta.id, id, newId)
      _ <- Delete(meta.id, id)
      _ <- Save(meta.id, actualId, documentWithId)
      _ <- Index.Delete(meta.id, id)
      _ <- Index.Put(meta.id, actualId, documentWithId)
    } yield noContent

}
