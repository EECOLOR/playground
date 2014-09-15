package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import org.qirx.cms.metadata.properties.Tag
import testUtils.ValidationResults
import testUtils.Example

class _08_03_Tag extends Specification with Example with ValidationResults {

  s"""|#${name[Tag]}
      |
      |The ${name[Tag]} class is intended for simple string based tags.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    val validate = Tag.validate(messages, _: Option[JsValue])

    "It has (by default) the id `tag`" - {
      Tag.id is "tag"
    }

    "It will not validate when no json is given" - {
      validate(None) is Some(requiredResult("tag"))
    }

    "It will not validate if the type of property is not a string" - {
      val result = validate(Some(obj()))
      result is Some(invalidTypeResult("tag"))
    }

    "It will not validate if the string is empty" - {
      validate(Some(JsString(""))) is Some(messageResult("tag", "empty"))
    }

    "It will not validate if the string contains special characters" - {
      validate(Some(JsString("*"))) is Some(messageResult("tag", "invalidTag"))
      validate(Some(JsString("test tag"))) is Some(messageResult("tag", "invalidTag"))
    }

    "It will validate for non empty strings that have the correct characters" - {
      validate(Some(JsString("valid_TAG-1_to_99"))) is None
    }

    "It's identifiable, it uses the tag itself as identity" - {
      Tag.determineIdentityOf(JsString("tag")) is "tag"
    }

    "Allows the use of other patterns" - new ExampleContainer {
      object CustomTag extends Tag("custom_tag", "[0-9]+")
    }.withSpecification { code =>

      val result = code.CustomTag.validate(messages, Some(JsString("test")))

      result is Some(
        messageResult("custom_tag", "invalidTag")
      )
    }

    "Provides the pattern in it's `toJson` method" - example {
      Tag.toJson is obj(
        "id" -> "tag",
        "extra" -> obj(
          "pattern" -> "[a-zA-Z0-9_-]+"
        )
      )
    }
  }
}