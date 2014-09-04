package testUtils

import org.qirx.cms.testing.MemoryIndex
import org.qirx.cms.construction.Index
import play.api.libs.json.Json.obj
import scala.concurrent.Future
import play.api.mvc.Results.Ok

class TestIndex extends MemoryIndex {

  import Index._

  override def transform[x] = {
    case Search(request, remainingPathSegments) =>
      val searchResult =
        obj(
          "info" -> s"Response from test index to search at `${remainingPathSegments.mkString}`"
        )

      Future.successful(Ok(searchResult))

    case Count(request, remainingPathSegments) =>
      val searchResult =
        obj(
          "info" -> s"Response from test index to count at `${remainingPathSegments.mkString}`"
        )

      Future.successful(Ok(searchResult))

    case other: Index[_] => super.transform[x](other)
  }
}