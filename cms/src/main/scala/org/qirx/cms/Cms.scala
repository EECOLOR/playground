package org.qirx.cms

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.higherKinds
import scala.language.implicitConversions

import org.qirx.cms.api.Api
import org.qirx.cms.api.MetadataApi
import org.qirx.cms.api.NoApi
import org.qirx.cms.api.PrivateApi
import org.qirx.cms.api.PublicApi
import org.qirx.cms.execution.AuthenticationToFuture
import org.qirx.cms.execution.DocumentIndexer
import org.qirx.cms.execution.DocumentValidator
import org.qirx.cms.evolution.EvolvingStore
import org.qirx.cms.execution.MetadataToId
import org.qirx.cms.metadata.DocumentMetadata

import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Action
import play.api.mvc.Handler
import play.api.mvc.RequestHeader

class Cms(
  pathPrefix: String,
  authenticate: RequestHeader => Future[Boolean],
  environment: Environment,
  documents: Seq[DocumentMetadata]) {

  private val documentMetadata = documents
  private val store = {
    val evolutions = documentMetadata.map(meta => meta.id -> meta.evolutions).toMap
    new EvolvingStore(environment.store, evolutions)
  }
  private val index = environment.index
  private val metadata = new MetadataToId(documentMetadata)
  private val authentication = new AuthenticationToFuture(authenticate)

  validateExistingDocuments()
  reindexExistingDocuments()
  
  /**
   * Only handle requests if the path starts with the given path prefix.
   */
  def handle(request: RequestHeader, orElse: RequestHeader => Option[Handler]) =
    if (request.path startsWith pathPrefix) Some(handleRequest)
    else orElse apply request

  private val handleRequest = Action.async { request =>

    val pathSegments = extractPathSegments(request.path)
    val api = determineApiFor(pathSegments.head)

    api.handleRequest(pathSegments.tail, request)
  }

  private def extractPathSegments(path: String): Seq[String] =
    path
      .replaceFirst(pathPrefix, "")
      .split("/")
      .filter(_.nonEmpty)

  private lazy val privateApi = new PrivateApi(store, index, metadata, authentication)
  private lazy val publicApi = new PublicApi(index, metadata)
  private lazy val metadataApi = new MetadataApi(metadata, authentication)

  private val determineApiFor: String => Api = {
    case "private" => privateApi
    case "public" => publicApi
    case "metadata" => metadataApi
    case _ => NoApi
  }

  /*
   * Using await in the methods below because we want these actions to be
   * completed before the CMS is started.
   */
  private def validateExistingDocuments() = {
    val validator = new DocumentValidator(metadata, store)
    
    val result = Await.result(validator.validate(), 60.seconds)

    result.foreach {
      case (document, meta, result) if result.nonEmpty =>
        environment.reportDocumentMetadataMismatch(document, meta, result)
      case _ => // nothing to report
    }
  }
  
  private def reindexExistingDocuments() = {
    val indexer = new DocumentIndexer(metadata, store, index)
    /*
     * Using await here because we want the documents to be indexed
     * before the CMS is started
     */
    Await.result(indexer.index(), 60.seconds)
  }
}