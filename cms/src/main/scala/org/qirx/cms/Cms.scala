package org.qirx.cms

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.language.higherKinds
import scala.language.implicitConversions
import org.qirx.cms.api.Api
import org.qirx.cms.api.MetadataApi
import org.qirx.cms.api.NoApi
import org.qirx.cms.api.PrivateApi
import org.qirx.cms.api.PublicApi
import org.qirx.cms.construction.Store
import org.qirx.cms.execution.AuthenticationRunner
import org.qirx.cms.execution.MetadataRunner
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata
import play.api.http.Status
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.mvc.Action
import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Results
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.construction.List
import org.qirx.cms.machinery.Free
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Validate
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.execution.SystemRunner
import scala.concurrent.Await
import org.qirx.cms.execution.VersionedStore

class Cms(
  pathPrefix: String,
  authenticate: RequestHeader => Future[Boolean],
  environment: Environment,
  documents: Seq[DocumentMetadata]) extends Results with Status
  with BuildTools with ExecutionTools {

  val executionContext = play.api.libs.concurrent.Execution.Implicits.defaultContext

  val store = new VersionedStore(environment.store, documents.map(meta => meta.id -> meta.evolutions).toMap)
  val metadata = new MetadataRunner(documents)
  val authentication = new AuthenticationRunner(authenticate)
  
  validateExistingDocuments()

  def handle(request: RequestHeader, orElse: RequestHeader => Option[Handler]) =
    if (request.path startsWith pathPrefix) Some(handleRequest)
    else orElse(request)

  private val handleRequest = Action.async { request =>

    val pathParts = extractPathParts(request.path)
    val api = determineApiFor(pathParts.head)

    api.handleRequest(pathParts.tail, request)
  }

  private def extractPathParts(path: String): Seq[String] =
    path
      .replaceFirst(pathPrefix, "")
      .split("/")
      .filter(_.nonEmpty)

  lazy val privateApi = new PrivateApi(
      store, 
          metadata, 
          authentication)

  private val determineApiFor: String => Api = {
    case "private" => privateApi
    case "public" => PublicApi
    case "metadata" => MetadataApi
    case _ => NoApi
  }

  def validateExistingDocuments() = {
    type Elements = ProgramType[(Base + Store + Metadata + Seq)#T]

    implicit class SeqEnhancements[T](s: Seq[T]) {
      def asProgram(implicit e: Elements) = toProgram(s)
    }

    def validationProgram(implicit e: Elements) = {
      for {
        meta <- documents.asProgram
        messages <- GetMessages(meta)
        documents <- List(meta.id, Set.empty)
        document <- documents.asProgram
        result <- Validate(meta, document, Set.empty, messages)
      } yield if (result.nonEmpty)
        environment.reportDocumentMetadataMismatch(document, meta, result)
    }

    type FutureSeq[T] = Future[Seq[T]]
    implicit def monad = new Free.Monad[FutureSeq] {
      def apply[A](a: A) = Future.successful(Seq(a))
      def flatMap[A, B](fa: FutureSeq[A], f: A => FutureSeq[B]) = {
        fa.flatMap { list =>
          Future.sequence(list.map(f)).map(_.flatten)
        }
      }
    }

    object SeqToFutureSeq extends (Seq ~> FutureSeq) {
      def transform[x] = x => Future.successful(x)
    }

    object FutureToFutureSeq extends (Future ~> FutureSeq) {
      def transform[x] = x => x.map(x => Seq(x))
    }

    val seqRunner = SeqToFutureSeq
    val metadataRunner = metadata andThen IdToFuture andThen FutureToFutureSeq
    val storeRunner = store andThen FutureToFutureSeq
    val systemRunner = SystemRunner andThen IdToFuture andThen FutureToFutureSeq

    val runner = seqRunner or metadataRunner or storeRunner or systemRunner

    val result = validationProgram.foldMap(runner)
    Await.result(result, 60.seconds)
  }
}