package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import org.qirx.cms.metadata.properties.Date
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import testUtils.Example
import testUtils.inApp

class _08_04_Date extends Specification with Example {

  s"""|#${name[Date]}
      |
      |The ${name[Date]} class is intended for ISO 8601 formatted dates.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    def validate(json: Option[JsValue]) = inApp {
      Date.validate(messages, json)
    }

    "It has (by default) the id `date`" - {
      Date.id is "date"
    }

    "It will not validate when no json is given" - example {
      validate(None) is Some(
        obj(
          "id" -> "date",
          "messageKey" -> "required",
          "message" -> "The property is required"
        )
      )
    }

    "It will not validate if the type of property is not a string" - example {
      validate(Some(obj())) is Some(
        obj(
          "id" -> "date",
          "error" -> "invalidType"
        )
      )
    }

    "It will not validate if the string is empty" - example {
      validate(Some(JsString(""))) is Some(
        obj(
          "id" -> "date",
          "messageKey" -> "invalidDate",
          "message" -> "The date `` is invalid"
        )
      )
    }

    "It will not validate if the string is not ISO 8601" - example {
      validate(Some(JsString("test"))) is Some(
        obj(
          "id" -> "date",
          "messageKey" -> "invalidDate",
          "message" -> "The date `test` is invalid"
        )
      )
    }

    "It will validate for ISO 8601 strings" - {
      validate(Some(JsString("2011-07-10T20:39:21+02:00"))) is None
    }

    "Allows the use of another id" - new ExampleContainer {
      object CustomDate extends Date("custom_date")
    }.withSpecification { code =>

      val result = inApp {
        code.CustomDate.validate(messages, Some(JsString("")))
      }

      result is Some(
        obj(
          "id" -> "custom_date",
          "messageKey" -> "invalidDate",
          "message" -> "The date `` is invalid"
        )
      )
    }
  }
}