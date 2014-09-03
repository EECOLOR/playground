package testUtils

import play.api.libs.ws.WSClient
import play.api.libs.ws.WSRequestHolder
import play.api.libs.ws.WSBody
import play.api.libs.ws.WSSignatureCalculator
import play.api.libs.ws.WSAuthScheme
import play.api.libs.ws.WSProxyServer
import play.api.libs.ws.WSResponse
import play.api.libs.iteratee.Enumerator
import play.api.libs.ws.WSResponseHeaders
import scala.concurrent.Future
import play.api.libs.ws.EmptyBody
import play.api.libs.ws.WSCookie
import scala.xml.Elem
import play.api.libs.json.JsValue
import play.api.libs.json.Json

class TestClient(response: WSResponse with WSResponseHeaders = TestResponse) extends WSClient {

  var lastRequestHolder:WSRequestHolder = null 
  
  def underlying[T]: T = ???

  def url(url: String) =
    TestRequestHolder(url)(response, setNewRequestHolder = lastRequestHolder = _)
}

case class TestRequestHolder(
  url: String,
  method: String = "GET",
  body: WSBody = EmptyBody,
  headers: Map[String, Seq[String]] = Map.empty,
  queryString: Map[String, Seq[String]] = Map.empty,
  calc: Option[WSSignatureCalculator] = None,
  auth: Option[(String, String, WSAuthScheme)] = None,
  followRedirects: Option[Boolean] = None,
  requestTimeout: Option[Int] = None,
  virtualHost: Option[String] = None,
  proxyServer: Option[WSProxyServer] = None)(
      implicit response: WSResponse with WSResponseHeaders,
      setNewRequestHolder:WSRequestHolder => Unit) extends WSRequestHolder {

  setNewRequestHolder(this)
  
  def sign(calc: WSSignatureCalculator) = copy(calc = Some(calc))
  def withAuth(username: String, password: String, scheme: WSAuthScheme) = copy(auth = Some((username, password, scheme)))
  def withHeaders(hdrs: (String, String)*) =
    copy(headers =
      hdrs.foldLeft(headers) {
        case (existingHeaders, (key, value)) =>
          existingHeaders + (key -> existingHeaders.get(key).map(_ :+ value).getOrElse(Seq(value)))
      })
  def withQueryString(parameters: (String, String)*) =
    copy(queryString =
      parameters.foldLeft(queryString) {
        case (existingQueryString, (key, value)) =>
          existingQueryString + (key -> existingQueryString.get(key).map(_ :+ value).getOrElse(Seq(value)))
      })
  def withFollowRedirects(follow: Boolean) = copy(followRedirects = Some(follow))
  def withRequestTimeout(timeout: Int) = copy(requestTimeout = Some(timeout))
  def withVirtualHost(vh: String) = copy(virtualHost = Some(vh))
  def withProxyServer(proxyServer: WSProxyServer) = copy(proxyServer = Some(proxyServer))
  def withBody(body: WSBody) = copy(body = body)
  def withMethod(method: String) = copy(method = method)

  def execute() = Future.successful(response)
  def stream() = ???
}

class TestResponseHeaders(val status: Int, val headers: Map[String, Seq[String]]) extends WSResponseHeaders
class TestResponse(
  status: Int = 200,
  headers: Map[String, Seq[String]] = Map.empty,
  val json: JsValue = Json.obj())
  extends TestResponseHeaders(status, headers) with WSResponse {

  val allHeaders = headers
  def header(key: String): Option[String] = headers.get(key).flatMap(_.headOption)

  def underlying[T]: T = ???
  def statusText = ???
  def cookies: Seq[WSCookie] = ???
  def cookie(name: String): Option[WSCookie] = ???
  def body: String = ???
  def xml: Elem = ???
}

object TestResponse extends TestResponse(200, Map.empty, Json.obj())