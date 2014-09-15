import org.qirx.cms.Cms
import org.joda.time.DateTimeUtils
import java.util.concurrent.atomic.AtomicInteger
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.macros.Location
import org.qirx.cms.metadata.dsl.Confidential
import org.qirx.cms.metadata.properties.RichContent
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.properties.Tag
import org.qirx.cms.metadata.properties.Date
import scala.concurrent.Future
import org.qirx.cms.metadata.dsl.Document
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import org.qirx.littlespec.fragments.Code
import scala.reflect.ClassTag
import org.qirx.littlespec.reporter.MarkdownReporter
import play.api.test.Helpers

package object testUtils {
  val cmsName = classOf[Cms].getSimpleName

  def withFixedDateTime[T](code: => T): T = this.synchronized {
    DateTimeUtils.setCurrentMillisFixed(1310323161323L)
    try code
    finally DateTimeUtils.setCurrentMillisSystem()
  }

  class CodeString[T](t: => T, code: String) {
    lazy val value = t
    override def toString = code
  }

  lazy val replacements = Seq(
    "testCms.value" -> testCms(new TestEnvironment).toString,
    "testCmsMetadata.value" -> testCmsMetadata.toString,
    "testCmsMetadataJson.value" -> testCmsMetadataJson.toString
  )

  def applyReplacements(code: String): String =
    replacements.foldLeft(code) {
      case (code, (string, replacement)) =>
        code.replaceAll(string, replacement)
    }

  def applyReplacements(code: Code): Code =
    Code(applyReplacements(code.text))

  def codeWithReplacements(implicit location: Location): Code =
    applyReplacements(Source codeAtLocation location)

  def codeString[T](code: => T)(implicit location: Location) =
    new CodeString(code, codeWithReplacements.text)

  def rawCodeString[T](code: => T)(implicit location: Location) =
    new CodeString(code, Source.codeAtLocation(location).text)

  def testCms(testEnvironment: TestEnvironment) = {
    val raw =
      rawCodeString {
        val article = testCmsMetadata.value

        new Cms(
          pathPrefix = "/api",
          authenticate = { _ => Future.successful(true) },
          environment = testEnvironment,
          documents = Seq(article)
        )
      }
    new CodeString(
      raw.value,
      raw.toString.replace("testCmsMetadata.value", testCmsMetadata.toString)
    )
  }

  lazy val testCmsMetadata = rawCodeString {
    Document(id = "article", idField = "title")(
      "title" -> Label,
      "secret" -> Confidential(Label.?),
      "body" -> RichContent.?,
      "tags" -> Tag.*,
      "date" -> Date.generated,
      "publishDate" -> Date.?
    )
  }

  lazy val testCmsMetadataJson = rawCodeString {
    obj(
      "id" -> "article",
      "properties" -> arr(
        obj(
          "id" -> "label",
          "name" -> "title"
        ),
        obj(
          "id" -> "label",
          "name" -> "secret",
          "optional" -> true,
          "confidential" -> true
        ),
        obj(
          "id" -> "rich_content",
          "name" -> "body",
          "optional" -> true,
          "extra" -> obj(
            "allowedElements" -> arr(
              "strong", "em", "ul", "ol", "li", "span[class|lang]",
              "a[href|hreflang|title|target]", "br", "p[class|lang]")
          )
        ),
        obj(
          "id" -> "tag",
          "name" -> "tags",
          "set" -> true,
          "nonEmpty" -> false,
          "extra" -> obj(
            "pattern" -> "[a-zA-Z0-9_-]+"
          )
        ),
        obj(
          "id" -> "date",
          "name" -> "date",
          "generated" -> true
        ),
        obj(
          "id" -> "date",
          "name" -> "publishDate",
          "optional" -> true
        )
      )
    )
  }

  def link[T: ClassTag] = {
    val fullyQualifiedName = implicitly[ClassTag[T]].runtimeClass.getName

    val name = MarkdownReporter.name(fullyQualifiedName)
    val fileName = MarkdownReporter.fileName(fullyQualifiedName)

    val cleanFileName = fileName.replaceAll("\\$", "")
    val cleanName = name.replaceAll("_", " ").replaceAll("\\$", "").trim

    s"[$cleanName]($cleanFileName)"
  }

  def moreInformation[T: ClassTag] =
    s"For detailed information see ${link[T]}"

  def name[C: ClassTag] = "`" + implicitly[ClassTag[C]].runtimeClass.getName + "`"

  def inApp[T](code: => T) = Helpers.running(TestApplication.fakeApplication())(code)
}