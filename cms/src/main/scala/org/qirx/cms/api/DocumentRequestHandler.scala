package org.qirx.cms.api

import scala.language.higherKinds
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Return
import org.qirx.cms.construction.ValueOf
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Store.Delete
import org.qirx.cms.construction.Store.Get
import org.qirx.cms.construction.Store.List
import org.qirx.cms.construction.Store.Save
import org.qirx.cms.construction.Store.Exists
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
import org.qirx.cms.machinery.BuildTools._
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.{ ~> => ~> }
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsObject
import play.api.mvc.AnyContent
import play.api.mvc.Request
import org.qirx.cms.construction.Branch
import play.api.mvc.Result
import org.qirx.cms.construction.Index
import org.qirx.cms.construction.RemoveConfidentialProperties
import org.qirx.cms.machinery.Free
import org.qirx.cms.machinery.Apply
import org.qirx.cms.machinery.Id
import org.qirx.cms.construction.DirectAction
import org.qirx.cms.machinery.Co
import org.qirx.cms.execution.SystemRunner
import org.qirx.cms.machinery.FlatMap
import org.qirx.cms.construction.AddGeneratedProperties
import org.qirx.cms.construction.api.GetFieldsFrom

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
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
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
      results <- Validate(meta, document, messages) ifEmpty postDocument(document)
      result <- ValitationResultsToResult(results)
    } yield result

  private def postDocument(document: JsObject) = {
    val id = meta.idGenerator.generateFor(document)
    for {
      uniqueId <- getUniqueId(id)
      documentWithId <- AddId(document, uniqueId)
      fullDocument <- AddGeneratedProperties(meta, documentWithId)
      _ <- save(uniqueId, fullDocument)
      result <- DocumentCreatedResult(uniqueId)
    } yield result
  }

  def put =
    for {
      (id, document) <- extractIdAndDocumentFromRequest
      _ <- Exists(meta.id, id) ifFalse Return(notFound)
      documentWithId <- AddId(document, id)
      messages <- GetMessages(meta)
      results <- Validate(meta, documentWithId, messages) ifEmpty putDocument(id, documentWithId)
      result <- ValitationResultsToResult(results)
    } yield result

  private def putDocument(id: String, document: JsObject) =
    for {
      fullDocument <- AddGeneratedProperties(meta, document)
      newId <- ExtractId(fullDocument)
      _ <- ValueOf(newId == id) ifFalse saveWithNewId(id, newId, fullDocument)
      _ <- save(id, fullDocument)
    } yield noContent

  def patch =
    for {
      (id, newDocument) <- extractIdAndDocumentFromRequest
      oldDocument <- Get(meta.id, id, Set.empty) ifNone Return(notFound)
      merged <- Merge(oldDocument, newDocument)
      fields <- GetFieldsFrom(newDocument)
      messages <- GetMessages(meta)
      results <- Validate(meta, merged, messages, fields) ifEmpty patchDocument(id, merged)
      result <- ValitationResultsToResult(results)
    } yield result

  private def patchDocument(id: String, document: JsObject) =
    for {
      newId <- ExtractId(document)
      _ <- ValueOf(newId == id) ifFalse saveWithNewId(id, newId, document)
      _ <- save(id, document)
    } yield noContent

  def delete =
    for {
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone deleteAll
      _ <- Exists(meta.id, id) ifFalse Return(notFound)
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
      _ <- Delete(meta.id, Some(id))
      _ <- Index.Delete(meta.id, Some(id))
    } yield noContent

  private def save(id: String, document: JsObject) =
    for {
      _ <- Save(meta.id, id, document)
      _ <- putInIndex(id, document)
    } yield ()

  private val makeUnique = meta.idGenerator.makeUnique _

  private def getUniqueId(id: String) = {
    val withBranch = uniqueIdProgram(id)
    val withoutBranch = withBranch.mergeBranch
    withoutBranch.mapSuspension[O]
  }

  private def uniqueIdProgram(id: String)(
    implicit e: ProgramType[(Base + Store + Branch[String]#T)#T]): Free[e.Result, String] = {
    for {
      _ <- Exists(meta.id, id) ifFalse ValueOf(id)
      uniqueId <- uniqueIdProgram(makeUnique(id))
    } yield uniqueId
  }

  private val deleteAll =
    for {
      _ <- Delete(meta.id)
      _ <- Index.Delete(meta.id)
    } yield noContent

  private val extractIdAndDocumentFromRequest =
    for {
      json <- ToJsValue(request) ifNone Return(badRequest)
      document <- ToJsObject(json) ifNone Return(jsonExpected)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone Return(notFound)
      _ <- Return(pathAfterId) ifNonEmpty Return(notFound)
    } yield (id, document)

  private def saveWithNewId(oldId: String, newId: String, document: JsObject) =
    for {
      _ <- SaveIdReference(meta.id, oldId, newId)
      _ <- save(newId, document)
      _ <- Delete(meta.id, Some(oldId))
      _ <- Index.Delete(meta.id, Some(oldId))
    } yield noContent

  private def putInIndex(id: String, document: JsObject) =
    for {
      publicDocument <- RemoveConfidentialProperties(meta, document)
      _ <- Index.Put(meta.id, id, publicDocument)
    } yield ()

}