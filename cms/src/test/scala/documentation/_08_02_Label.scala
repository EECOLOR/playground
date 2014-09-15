package documentation

import org.qirx.littlespec.Specification
import testUtils.name
import org.qirx.cms.metadata.properties.Label
import play.api.libs.json.Json.obj
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsValue
import play.api.libs.json.JsString
import testUtils.ValidationResults
import testUtils.Example

class _08_02_Label extends Specification with Example with ValidationResults {

  s"""|#${name[Label]}
      |
      |The ${name[Label]} class is intended for simple strings.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")
    val validate = Label.validate(messages, _: Option[JsValue])

    "It has (by default) the id `label`" - {
      Label.id is "label"
    }

    "It will not validate when no json is given" - {
      validate(None) is Some(requiredResult("label"))
    }

    "It will not validate if the type of property is not a string" - {
      val result = validate(Some(obj()))
      result is Some(invalidTypeResult("label"))
    }

    "It will not validate if the string is empty" - {
      validate(Some(JsString(""))) is Some(messageResult("label", "empty"))
    }

    "It will validate for non empty strings" - {
      validate(Some(JsString("non empty"))) is None
    }
    
    "Allows the use of another id" - new ExampleContainer {
      object CustomLabel extends Label("custom_label")
    }.withSpecification { code =>

      val result = code.CustomLabel.validate(messages, Some(JsString("")))

      result is Some(
        messageResult("custom_label", "empty")
      )
    }
  }
}