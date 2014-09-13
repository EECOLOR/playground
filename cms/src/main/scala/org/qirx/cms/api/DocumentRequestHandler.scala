package org.qirx.cms.api

import scala.language.higherKinds

import org.qirx.cms.construction._
import org.qirx.cms.construction.api._
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.Program
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata

import play.api.libs.json.JsObject
import play.api.mvc.AnyContent
import play.api.mvc.Request
import play.api.mvc.Result

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
    e5: Branch[Result]#T ~> O) {

  import BuildTools._
  import Results._

  private val makeIdUnique = meta.idGenerator.makeUnique _
  private val generateIdFor = meta.idGenerator.generateFor _
  private val metaId = meta.id
  
  def get =
    for {
      fieldSet <- GetFieldSetFromQueryString(request.queryString)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone list(fieldSet)
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
      document <- Store.Get(metaId, id, fieldSet) ifNone Return(notFound)
    } yield ok(document)

  private def list(fieldSet: Set[String]) =
    for {
      documents <- Store.List(metaId, fieldSet)
    } yield ok(documents)

  def post =
    for {
      _ <- GetNextSegment(pathAtDocument) ifSome Return(notFound)
      json <- ToJsValue(request) ifNone Return(badRequest)
      document <- ToJsObject(json) ifNone Return(jsonExpected)
      messages <- GetMessages(meta)
      results <- Validate(meta, document, messages) ifEmpty postDocument(document)
    } yield valitationResultsToResult(results)

  private def postDocument(document: JsObject) = {
    val id = generateIdFor(document)
    for {
      uniqueId <- getUniqueId(id)
      documentWithId <- AddId(document, uniqueId)
      fullDocument <- AddGeneratedProperties(meta, documentWithId)
      _ <- save(uniqueId, fullDocument)
    } yield created(uniqueId)
  }

  def put =
    for {
      (id, document) <- extractIdAndDocumentFromRequest
      _ <- Store.Exists(metaId, id) ifFalse Return(notFound)
      messages <- GetMessages(meta)
      results <- Validate(meta, document, messages) ifEmpty putDocument(id, document)
    } yield valitationResultsToResult(results)

  private def putDocument(id: String, document: JsObject) =
    for {
      fullDocument <- AddGeneratedProperties(meta, document)
      generatedId <- ValueOf(generateIdFor(fullDocument))
      documentWithId <- AddId(fullDocument, generatedId)
      _ <- ValueOf(id == generatedId) ifFalse saveWithNewId(id, generatedId, documentWithId)
      _ <- save(id, documentWithId)
    } yield noContent

  def patch =
    for {
      (id, newDocument) <- extractIdAndDocumentFromRequest
      oldDocument <- Store.Get(metaId, id) ifNone Return(notFound)
      merged <- Merge(oldDocument, newDocument)
      messages <- GetMessages(meta)
      fields <- GetFieldsFrom(newDocument)
      results <- Validate(meta, merged, messages, fields) ifEmpty patchDocument(id, merged)
    } yield valitationResultsToResult(results)

  private def patchDocument(id: String, document: JsObject) =
    for {
      fullDocument <- AddGeneratedProperties(meta, document)
      generatedId <- ValueOf(generateIdFor(fullDocument))
      documentWithId <- AddId(fullDocument, generatedId)
      _ <- ValueOf(id == generatedId) ifFalse saveWithNewId(id, generatedId, fullDocument)
      _ <- save(id, fullDocument)
    } yield noContent

  def delete =
    for {
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone deleteAll
      _ <- Store.Exists(metaId, id) ifFalse Return(notFound)
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
      _ <- Store.Delete(metaId, id)
      _ <- Index.Delete(metaId, id)
    } yield noContent

  private val deleteAll =
    for {
      _ <- Store.DeleteAll(metaId)
      _ <- Index.DeleteAll(metaId)
    } yield noContent

  private val extractIdAndDocumentFromRequest =
    for {
      json <- ToJsValue(request) ifNone Return(badRequest)
      document <- ToJsObject(json) ifNone Return(jsonExpected)
      (id, pathAfterId) <- GetNextSegment(pathAtDocument) ifNone Return(notFound)
      _ <- ValueOf(pathAfterId) ifNonEmpty Return(notFound)
    } yield (id, document)

  private def save(id: String, document: JsObject) =
    for {
      _ <- Store.Save(metaId, id, document)
      _ <- putInIndex(id, document)
    } yield ()

  private def saveWithNewId(oldId: String, newId: String, document: JsObject) =
    for {
      _ <- Store.AddId(metaId, oldId, newId)
      _ <- Index.AddId(metaId, oldId, newId)
      _ <- save(newId, document)
    } yield ok(idObj(newId))

  private def putInIndex(id: String, document: JsObject) =
    for {
      publicDocument <- RemoveConfidentialProperties(meta, document)
      _ <- Index.Put(metaId, id, publicDocument)
    } yield ()

  private def getUniqueId(id: String) = {
    val withBranch = uniqueIdProgram(id)
    val withoutBranch = withBranch.mergeBranch
    withoutBranch.changeInstructionTypeTo[O]
  }

  private type Elements = ProgramType[(System + Store + Branch[String]#T)#T]
  private def uniqueIdProgram(id: String)(implicit e: Elements): Program[e.Result, String] = {
    for {
      _ <- Store.Exists(metaId, id) ifFalse ValueOf(id)
      uniqueId <- uniqueIdProgram(makeIdUnique(id))
    } yield uniqueId
  }
}
