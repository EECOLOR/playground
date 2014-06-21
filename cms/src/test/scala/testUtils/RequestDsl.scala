package testUtils

import play.api.libs.json.JsValue
import play.api.mvc.Request
import play.api.http.Writeable
import play.api.Application
import play.api.test.Helpers
import akka.util.Timeout
import play.api.test.FakeRequest
import scala.concurrent.duration._

trait RouteRequest {
  val app: Application

  def routeRequest[T: Writeable](request: Request[T]) =
    Helpers.running(app) {
      val result = Helpers.route(app, request).get

      implicit val timeout = Timeout(2.seconds)
      (Helpers.status(result), Helpers.contentAsJson(result))
    }
}

class PostToApplication(val app: Application, pathPrefix: String = "") extends RouteRequest {
  class WithTo[T: Writeable](body: T, header: Option[(String, String)] = None) {

    def to(path: String) = {
      val request = FakeRequest("POST", pathPrefix + path)
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

class GetFromApplication(val app: Application) extends RouteRequest {

  def from(path: String) = {
    val request = FakeRequest("POST", path)
    import Helpers._
    routeRequest(request)
  }
}