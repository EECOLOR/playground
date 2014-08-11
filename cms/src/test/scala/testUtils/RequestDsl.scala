package testUtils

import play.api.libs.json.JsValue
import play.api.mvc.Request
import play.api.http.Writeable
import play.api.Application
import play.api.test.Helpers
import akka.util.Timeout
import play.api.test.FakeRequest
import scala.concurrent.duration._
import play.api.libs.json.Json

trait RouteRequest {
  val application: Application

  def routeRequest[T: Writeable](request: Request[T]) =
    Helpers.running(application) {
      val result = Helpers.route(application, request).get

      implicit val timeout = Timeout(2.seconds)

      val content = Helpers.contentAsString(result)
      val json = Option(content).filter(_.nonEmpty).map(Json.parse)

      (Helpers.status(result), json.orNull)
    }
}

abstract class WithBodyToApplication(
  method: String,
  val application: Application,
  pathPrefix: String) extends RouteRequest {

  class WithTo[T: Writeable](body: T, header: Option[(String, String)] = None) {

    val at = to _

    def to(path: String) = {
      val request = FakeRequest(method, pathPrefix + path)
        .withHeaders(header.toSeq: _*)
        .withBody(body)

      routeRequest(request)
    }
  }

  def apply[T: Writeable](body: T) =
    new WithTo(body) {
      def withHeader(header: (String, String)) = new WithTo(body, Some(header))
    }
}

abstract class ToApplicationWithBody(
  method: String,
  val application: Application,
  pathPrefix: String) extends RouteRequest {

  class WithUsing(path: String) {
    def using[T: Writeable](body: T) = {
      val request = FakeRequest(method, pathPrefix + path)
        .withBody(body)

      routeRequest(request)
    }
  }

  def apply(path: String) = new WithUsing(path)
}

class WithHeader(
  val application: Application,
  method: String,
  pathPrefix: String,
  header: Option[(String, String)] = None) extends RouteRequest {

  def from(path: String) = {
    val request = FakeRequest(method, pathPrefix + path)
      .withHeaders(header.toSeq: _*)
    import Helpers._
    routeRequest(request)
  }
}

abstract class FromApplication(method: String, application: Application, pathPrefix: String)
  extends WithHeader(application, method, pathPrefix) {

  def withHeader(header: (String, String)) =
    new WithHeader(application, method, pathPrefix, Some(header))
}

class PostToApplication(app: Application, pathPrefix: String = "")
  extends WithBodyToApplication("POST", app, pathPrefix)

class PutToApplication(app: Application, pathPrefix: String = "")
  extends WithBodyToApplication("PUT", app, pathPrefix)

class PatchToApplication(app: Application, pathPrefix: String = "")
  extends ToApplicationWithBody("PATCH", app, pathPrefix)

class GetFromApplication(app: Application, pathPrefix: String = "")
  extends FromApplication("GET", app, pathPrefix)

class DeleteFromApplication(app: Application, pathPrefix: String = "")
  extends FromApplication("DELETE", app, pathPrefix)