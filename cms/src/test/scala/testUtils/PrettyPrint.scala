package testUtils

import play.api.libs.json.JsObject
import play.api.libs.json.Json

trait PrettyPrint[T] {
  def print(t: T): String
}
object PrettyPrint {
  def apply[T](f:T => String) = new PrettyPrint[T] {
    def print(t:T) = f(t)
  }
  
  implicit def forBoolean = PrettyPrint[Boolean](_.toString)
  
  implicit def forString = PrettyPrint[String]("\"" + _ + "\"")
  
  implicit def forJsValue = PrettyPrint[JsObject](Json.prettyPrint)
  
  implicit def forSeq[T](implicit pretty: PrettyPrint[T]) =
    PrettyPrint[Seq[T]] { _.map(pretty print _).toString }

  implicit def forOpt[T](implicit pretty: PrettyPrint[T]) = 
    PrettyPrint[Option[T]] { _.map(pretty print _).toString } 
  
  implicit def forMap[T](implicit pretty: PrettyPrint[T]) =
    PrettyPrint[Map[String, T]] {
      _.map {
        case (key, value) => key -> (pretty print value)
      }.toString
    }
}