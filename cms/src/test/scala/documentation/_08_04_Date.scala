package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import org.qirx.cms.metadata.properties.Date
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import testUtils.ValidationResults
import testUtils.Example

class _08_04_Date extends Specification with Example with ValidationResults {

  s"""|#${name[Date]}
      |
      |The ${name[Date]} class is intended for ISO 8601 formatted dates.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    val validate = Date.validate(messages, _: Option[JsValue])

    "It has (by default) the id `date`" - {
      Date.id is "date"
    }

    "It will not validate when no json is given" - {
      validate(None) is Some(requiredResult("date"))
    }

    "It will not validate if the type of property is not a string" - {
      val result = validate(Some(obj()))
      result is Some(invalidTypeResult("date"))
    }

    "It will not validate if the string is empty" - {
      validate(Some(JsString(""))) is Some(messageResult("date", "invalidDate"))
    }
    
    "It will not validate if the string is not ISO 8601" - {
      validate(Some(JsString("test"))) is Some(messageResult("date", "invalidDate"))
    }

    "It will validate for ISO 8601 strings" - {
      validate(Some(JsString("2011-07-10T20:39:21+02:00"))) is None
    }
    
    "Allows the use of another id" - new ExampleContainer {
      object CustomDate extends Date("custom_date")
    }.withSpecification { code =>

      val result = code.CustomDate.validate(messages, Some(JsString("")))

      result is Some(
        messageResult("custom_date", "invalidDate")
      )
    }
  }
}