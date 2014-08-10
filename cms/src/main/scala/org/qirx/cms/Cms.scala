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
import org.qirx.cms.execution.AuthenticationRunner
import org.qirx.cms.execution.DocumentValidator
import org.qirx.cms.execution.EvolvingStore
import org.qirx.cms.execution.MetadataRunner
import org.qirx.cms.metadata.DocumentMetadata
import play.api.http.Status
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Action
import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Results
import org.qirx.cms.execution.DocumentIndexer

class Cms(
  pathPrefix: String,
  authenticate: RequestHeader => Future[Boolean],
  environment: Environment,
  documents: Seq[DocumentMetadata]) extends Results with Status {

  private val store = {
    val evolutions = documents.map(meta => meta.id -> meta.evolutions).toMap
    new EvolvingStore(environment.store, evolutions)
  }
  private val index = environment.index
  private val metadata = new MetadataRunner(documents)
  private val authentication = new AuthenticationRunner(authenticate)

  validateExistingDocuments()
  reindexExistingDocuments()
  
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
  private lazy val publicApi = new PublicApi(index, store, metadata)

  private val determineApiFor: String => Api = {
    case "private" => privateApi
    case "public" => publicApi
    case "metadata" => MetadataApi
    case _ => NoApi
  }

  private def validateExistingDocuments() = {
    val validator = new DocumentValidator(documents, metadata, store)

    /*
     * Using await here because we want the documents to be validated 
     * before the CMS is started
     */
    val result = Await.result(validator.validate(), 60.seconds)

    result.foreach {
      case (document, meta, result) if result.nonEmpty =>
        environment.reportDocumentMetadataMismatch(document, meta, result)
      case _ => // nothing to report
    }
  }
  
  private def reindexExistingDocuments() = {
    val indexer = new DocumentIndexer(documents, store, index)
    /*
     * Using await here because we want the documents to be indexed
     * before the CMS is started
     */
    Await.result(indexer.index(), 60.seconds)
  }
}