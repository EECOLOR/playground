package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import org.qirx.cms.metadata.properties.RichContent
import testUtils.Example
import testUtils.inApp
import play.api.libs.json.JsArray
import org.qirx.cms.metadata.properties.RichContentElement
import org.qirx.littlespec.fragments.Fragment
import testUtils.TestApplication.fakeApplication
import play.api.test.Helpers.running

class _08_05_RichContent extends Specification with Example {

  s"""|#${name[RichContent]}
      |
      |The ${name[RichContent]} class is intended for rich html content.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    def validate(json: Option[JsValue]) = inApp {
      RichContent.validate(messages, json)
    }

    "It has (by default) the id `rich_content`" - {
      RichContent.id is "rich_content"
    }

    "It will not validate when no json is given" - example {
      validate(None) is Some(
        obj(
          "id" -> "rich_content",
          "messageKey" -> "required",
          "message" -> "The property is required"
        )

      )
    }

    "It will not validate if the type of property is not an array" - {
      val result = validate(Some(obj()))
      result is Some(
        obj(
          "id" -> "rich_content",
          "error" -> "invalidType"
        )
      )
    }

    "It will validate when the array is empty" - {
      validate(Some(arr())) is None
    }

    """|It will not validate in the case of: 
       |- An invalid structure
       |- An element that is not allowed
       |- An attribute that is not allowed""".stripMargin - example {
      val result = validate(Some(
        arr(
          obj("element" -> "strong"),
          obj("element" -> "h1"),
          arr("incorrect element"),
          obj("incorrect" -> "element"),
          obj(
            "element" -> "strong",
            "attributes" -> obj("not_class" -> "value", "not" -> "")
          )
        )
      ))

      result is Some(
        obj(
          "id" -> "rich_content",
          "errors" -> arr(
            obj(
              "messageKey" -> "elementNotAllowed",
              "message" -> "The element `h1` is not allowed"
            ),
            obj(
              "messageKey" -> "invalidElement",
              "message" -> "The element `[\"incorrect element\"]` is invalid"
            ),
            obj(
              "messageKey" -> "invalidElement",
              "message" -> "The element `{\"incorrect\":\"element\"}` is invalid"
            ),
            obj(
              "messageKey" -> "attributesNotAllowed",
              "message" -> "The attribute(s) `not_class` and `not` is/are not allowed"
            )
          )
        )
      )
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

      val result = inApp {
        code.CustomRichContent.validate(messages, Some(
          arr(
            obj("element" -> "strong")
          )
        ))
      }

      result is Some(
        obj(
          "id" -> "custom_rich_content",
          "errors" -> arr(
            obj(
              "messageKey" -> "elementNotAllowed",
              "message" -> "The element `strong` is not allowed"
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