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
  val app: Application

  def routeRequest[T: Writeable](request: Request[T]) =
    Helpers.running(app) {
      val result = Helpers.route(app, request).get

      implicit val timeout = Timeout(2.seconds)
      
      val content = Helpers.contentAsString(result)
      val json = Option(content).filter(_.nonEmpty).map(Json.parse)
      
      (Helpers.status(result), json.orNull)
    }
}

abstract class WithBodyToApplication(method: String, val app: Application, pathPrefix: String) extends RouteRequest {
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

class PostToApplication(app: Application, pathPrefix: String = "")
  extends WithBodyToApplication("POST", app, pathPrefix)

class PutToApplication(app: Application, pathPrefix: String = "")
  extends WithBodyToApplication("PUT", app, pathPrefix)

class GetFromApplication(val app: Application, pathPrefix: String = "") extends RouteRequest {

  def from(path: String) = {
    val request = FakeRequest("GET", pathPrefix + path)
    import Helpers._
    routeRequest(request)
  }
}