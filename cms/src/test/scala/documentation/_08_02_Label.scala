package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import org.qirx.cms.metadata.properties.Label
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import testUtils.Example
import play.api.Play
import org.qirx.littlespec.fragments.Fragment
import testUtils.inApp

class _08_02_Label extends Specification with Example {

  s"""|#${name[Label]}
      |
      |The ${name[Label]} class is intended for simple strings.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    def validate(json: Option[JsValue]) = inApp {
      Label.validate(messages, json)
    }

    "It has (by default) the id `label`" - {
      Label.id is "label"
    }

    "It will not validate when no json is given" - example {
      validate(None) is Some(
        obj(
          "id" -> "label",
          "messageKey" -> "required",
          "message" -> "The property is required"
        )
      )
    }

    "It will not validate if the type of property is not a string" - example {
      validate(Some(obj())) is Some(
        obj(
          "id" -> "label",
          "error" -> "invalidType"
        )
      )
    }

    "It will not validate if the string is empty" - example {
      validate(Some(JsString(""))) is Some(
        obj(
          "id" -> "label",
          "messageKey" -> "empty",
          "message" -> "The property may not be empty"
        )
      )
    }

    "It will validate for non empty strings" - {
      validate(Some(JsString("non empty"))) is None
    }

    "Allows the use of another id" - new ExampleContainer {
      object CustomLabel extends Label("custom_label")
    }.withSpecification { code =>

      val result = inApp { 
        code.CustomLabel.validate(messages, Some(JsString(""))) 
      }

      result is Some(
        obj(
          "id" -> "custom_label",
          "messageKey" -> "empty",
          "message" -> "The property may not be empty"
        )
      )
    }
  }
}