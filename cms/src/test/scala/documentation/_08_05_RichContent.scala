package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import org.qirx.cms.metadata.properties.RichContent
import testUtils.ValidationResults
import testUtils.Example
import play.api.libs.json.JsArray
import org.qirx.cms.metadata.properties.RichContentElement

class _08_05_RichContent extends Specification with Example with ValidationResults {

  s"""|#${name[RichContent]}
      |
      |The ${name[RichContent]} class is intended for rich html content.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    val validate = RichContent.validate(messages, _: Option[JsValue])

    "It has (by default) the id `rich_content`" - {
      RichContent.id is "rich_content"
    }

    "It will not validate when no json is given" - {
      validate(None) is Some(requiredResult("rich_content"))
    }

    "It will not validate if the type of property is not an array" - {
      val result = validate(Some(obj()))
      result is Some(invalidTypeResult("rich_content"))
    }

    "It will validate when the array is empty" - {
      validate(Some(arr())) is None
    }

    "It will validate when contents are correct" - {
      validate(Some(arr(
        obj("element" -> "span", "text" -> "The title"),
        obj(
          "element" -> "p",
          "attributes" -> obj("class" -> "green"),
          "text" -> "Some text"
        ),
        obj(
          "element" -> "p",
          "attributes" -> obj("class" -> "red"),
          "children" -> arr(
            "Some ",
            obj("element" -> "strong", "text" -> "special"),
            "text"
          )
        ),
        "For correct semantics this should probably be in a `p`"
      ))) is None
    }

    "Allows the use of other allowed elements" - new ExampleContainer {
      object CustomRichContent extends RichContent("custom_rich_content", Seq(
        RichContentElement("p", Seq("class")),
        RichContentElement("div", Seq("class")),
        RichContentElement("span", Seq("class"))
      ))
    }.withSpecification { code =>

      val result = code.CustomRichContent.validate(messages, Some(
        arr(
          obj("element" -> "strong"),
          obj(),
          obj("element" -> "p", "attributes" -> obj("not_class" -> "value")))
      ))

      result is Some(
        obj(
          "id" -> "custom_rich_content",
          "errors" -> arr(
            obj(
              "messageKey" -> "elementNotAllowed",
              "message" -> "testProperty.elementNotAllowed"
            ),
            obj(
              "messageKey" -> "invalidElement",
              "message" -> "testProperty.invalidElement"
            ),
            obj(
              "messageKey" -> "attributesNotAllowed",
              "message" -> "testProperty.attributesNotAllowed"
            )
          )
        )
      )
    }

    "Provides the allowed elements in it's `toJson` method" - example {
      RichContent.toJson is obj(
        "id" -> "rich_content",
        "extra" -> obj(
          "allowedElements" -> arr(
            "strong", "em", "ul", "ol", "li", "span[class|lang]",
            "a[href|hreflang|title|target]", "br", "p[class|lang]"
          )
        )
      )
    }
  }
}