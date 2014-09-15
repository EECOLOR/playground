package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import org.qirx.cms.metadata.properties.Tag
import testUtils.Example
import testUtils.inApp

class _08_03_Tag extends Specification with Example {

  s"""|#${name[Tag]}
      |
      |The ${name[Tag]} class is intended for simple string based tags.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    def validate(json: Option[JsValue]) = inApp {
      Tag.validate(messages, json)
    }
    
    "It has (by default) the id `tag`" - {
      Tag.id is "tag"
    }

    "It will not validate when no json is given" - example {
      validate(None) is Some(
        obj(
          "id" -> "tag",
          "messageKey" -> "required",
          "message" -> "The property is required"
        )
      )
    }

    "It will not validate if the type of property is not a string" - {
      validate(Some(obj())) is Some(
        obj(
          "id" -> "tag",
          "error" -> "invalidType"
        )
      )
    }

    "It will not validate if the string is empty" - example {
      validate(Some(JsString(""))) is Some(
        obj(
          "id" -> "tag",
          "messageKey" -> "empty",
          "message" -> "The property may not be empty"
        )
      )
    }

    "It will not validate if the string contains special characters" - example {
      validate(Some(JsString("test tag"))) is Some(
        obj(
          "id" -> "tag",
          "messageKey" -> "invalidTag",
          "message" -> "The tag `test tag` contains invalid characters"
        )
      )
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

      val result = inApp {
        code.CustomTag.validate(messages, Some(JsString("test")))
      }

      result is Some(
        obj(
          "id" -> "custom_tag",
          "messageKey" -> "invalidTag",
          "message" -> "The tag `test` contains invalid characters"
        )
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