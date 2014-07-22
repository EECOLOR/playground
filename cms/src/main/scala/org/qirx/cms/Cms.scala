package org.qirx.cms

import org.qirx.cms.metadata.DocumentMetadata
import play.api.mvc.Request
import play.api.mvc.Result
import scala.concurrent.Future
import play.api.mvc.AnyContent
import play.api.mvc.Handler
import play.api.mvc.RequestHeader
import play.api.mvc.Action
import play.api.mvc.Controller
import play.api.mvc.Results
import play.api.libs.json.JsNull
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import play.api.http.Status
import org.qirx.cms.api.Api
import org.qirx.cms.api.PrivateApi
import org.qirx.cms.api.MetadataApi
import org.qirx.cms.api.NoApi
import org.qirx.cms.api.PublicApi
import org.qirx.cms.api.PrivateApi2
import org.qirx.cms.execution.MetadataHandler
import org.qirx.cms.execution.SystemHandler
import org.qirx.cms.machinery.Apply
import org.qirx.cms.machinery.Free
import scala.language.implicitConversions
import scala.language.higherKinds
import org.qirx.cms.machinery.Co
import org.qirx.cms.machinery.Coproduct
import org.qirx.cms.construction.Branch
import org.qirx.cms.execution.AuthenticationHandler
import org.qirx.cms.construction.System
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Authentication
import org.qirx.cms.machinery.~>
import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.ProgramRunner
import org.qirx.cms.machinery.FutureResultBranch

class Cms(
  pathPrefix: String,
  authenticate: RequestHeader => Future[Boolean],
  documents: Seq[DocumentMetadata])(implicit store: Store ~> Future) extends Results with Status {

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

  private val determineApiFor: String => Api = {
    case "private" =>
      import machinery.Id
      import machinery.Program

      object IdTransformation {
        def apply[F[_]] = new (F ~> F) {
          def transform[x] = x => x
        }
      }

      implicit class NaturalTransformationEnhancements[F[_], G[_], X](fToG: X)(
        implicit xAsTransformation: X => F ~> G) {

        def or[H[_], T[_]](hToT: H ~> T)(implicit hToG: H ~> T => H ~> G) =
          new (Co[H, F]#T ~> G) {
            def transform[x] =
              _.value match {
                case Left(head) => hToG(hToT) apply head
                case Right(tail) => fToG[x](tail)
              }
          }
      }

      def toFuture = new (Branch[Result]#Instance ~> FutureResultBranch) {
        def transform[x] = x => FutureResultBranch(Future successful x)
      }

      def toBranch = new (Id ~> Branch[Result]#Instance) {
        def transform[x] = x => Branch[Result].Instance(Left(x))
      }

      def toFutureBranch = new (Future ~> FutureResultBranch) {
        def transform[x] = x => FutureResultBranch(x map toBranch.apply)
      }

      val branching = IdTransformation[Branch[Result]#Instance] andThen toFuture
      val system = SystemHandler andThen toBranch andThen toFuture
      val metadata = new MetadataHandler(documents) andThen toBranch andThen toFuture
      val authentication = new AuthenticationHandler(authenticate) andThen toFutureBranch
      val storage = store andThen toFutureBranch

      implicit val runner = 
        system or
        storage or
        metadata or
        authentication or
        branching

      import Coproduct.Transformations._
      new PrivateApi2(ProgramRunner fromRunner runner)
    case "public" => PublicApi
    case "metadata" => MetadataApi
    case _ => NoApi
  }
}