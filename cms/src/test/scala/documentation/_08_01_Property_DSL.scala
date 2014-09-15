package documentation

import scala.util.Either.MergeableEither

import org.qirx.cms.i18n.Messages
import org.qirx.cms.metadata.dsl.Confidential
import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.GeneratedValueProperty
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.dsl.OptionalValueProperty
import org.qirx.cms.metadata.dsl.Property
import org.qirx.cms.metadata.dsl.PropertyValidation
import org.qirx.cms.metadata.dsl.ValueSetProperty
import org.qirx.littlespec.Specification

import play.api.libs.json.JsObject
import play.api.libs.json.JsString
import play.api.libs.json.JsValue
import play.api.libs.json.Json.arr
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.toJsFieldJsValueWrapper
import testUtils.Example
import testUtils.name
import testUtils.inApp

class _08_01_Property_DSL extends Specification with Example {

  """|#Property DSL
     |
     |The property DSL consists of multiple traits and abstract classes 
     |that help discoverability of options.""".stripMargin - {

    val messages = Messages.withPrefix("testProperty")

    s"""|##${name[Property]}
        |
        |This is a class that can be used as a base for properties, it simplifies 
        |the implementation of the `validate` and `toJson` methods. It also mixes 
        |in the ${name[PropertyValidation]} trait which supplies utility methods
        |that help with validation.""".stripMargin - new ExampleContainer {
      object MyProperty extends Property("testProperty") {
        def validate(messages: Messages, value: JsValue): Option[JsObject] = None
        def extraJson: Option[JsObject] = Some(obj("my" -> "property"))
      }
    }.withSpecification { code =>
      val MyProperty = code.MyProperty

      def validate(json: Option[JsValue]) = inApp {
        MyProperty.validate(messages, json)
      }
      
      "The `validate` method now reports an error when no value is given" - example {
        validate(None) is Some(
          obj(
            "id" -> "testProperty",
            "messageKey" -> "required",
            "message" -> "The property is required"
          )
        )
      }

      "The `toJson` method adds the extra json" - example {
        val result = MyProperty.toJson
        result is obj(
          "id" -> "testProperty",
          "extra" -> obj("my" -> "property")
        )
      }
    }

    s"""|##${name[PropertyValidation]}
        |
        |""".stripMargin - {}

    class TestProperty extends Property("testProperty") {
      def validate(messages: Messages, value: JsValue): Option[JsObject] = None
      def extraJson: Option[JsObject] = None
    }
    object TestProperty extends TestProperty

    s"""|##`?` (optional)
        |
        |The ${name[Property]} class provides a `?` by default. Calling it will wrap 
        |the current property into an ${name[OptionalValueProperty[_]]}.""".stripMargin - new ExampleContainer {
      val optionalTestProperty: OptionalValueProperty[TestProperty.type] = TestProperty.?
    }.withSpecification { code =>
      val optionalTestProperty = code.optionalTestProperty

      s"""|The ${name[OptionalValueProperty[_]]} class will implement the validation method
          |so that it will not report anything if the value is not given.""".stripMargin - {
        val result = optionalTestProperty.validate(messages, None)
        result is None
      }

      "It will also add extra information to the `toJson` method" - example {
        val result = optionalTestProperty.toJson
        result is obj(
          "id" -> "testProperty",
          "optional" -> true
        )
      }
    }

    s"""|##`generated`
        |
        |The `generated` method is provided by the ${name[GeneratableValue]} trait. When 
        |you mix this trait with your property it requires you to specify a method that 
        |generates the value. Calling it will wrap the current property into a 
        |${name[GeneratedValueProperty[_]]}.""".stripMargin - new ExampleContainer {
      object Prop extends TestProperty with GeneratableValue {
        def generate = JsString("something")
      }
      val generatedProperty: GeneratedValueProperty[Prop.type] = Prop.generated
    }.withSpecification { code =>
      val generatedProperty = code.generatedProperty

      def validate(json: Option[JsValue]) = inApp {
        generatedProperty.validate(messages, json)
      }

      "This will provide a generator that will generate the property." - {
        val Some(generator) = generatedProperty.generator
        generator.generate("", obj()) is JsString("something")
      }

      """|Note that the provided generator will also validate the generated property,
         |a failure during validation results in a runtime error. This is done to keep
         |the API simple. This means you must always create a test that tests the 
         |generator you provide for custom properties.""".stripMargin - {
        val property = new Property("test") with GeneratableValue {
          def validate(messages: Messages, value: JsValue): Option[JsObject] =
            if (value != JsString("something else")) Some(obj("validation" -> "error"))
            else None
          def generate = JsString("something")
          def extraJson = None
        }.generated

        val Some(generator) = property.generator
        generator.generate("", obj()) must throwA[RuntimeException].like {
          case e => e.getMessage is
            """|Generated value is invalid, value: "something"
               |validation results: Some({"validation":"error"})""".stripMargin
        }
      }

      "If the value is given, it will not validate".stripMargin - example {
        validate(Some(obj())) is Some(
          obj(
            "id" -> "testProperty",
            "error" -> "generated"
          )
        )
      }

      "It will also add extra information to the `toJson` method" - example {
        val result = generatedProperty.toJson
        result is obj(
          "id" -> "testProperty",
          "generated" -> true
        )
      }
    }

    s"""|##`*` (zore or more)
        |
        |The `*` method is provided by the ${name[Identifiable]} trait. It effectively 
        |turns your property into an array. When you mix this trait with your property 
        |it requires you to specify a method that determines the ideneity of the value.
        |Calling it will wrap the current property into a ${name[ValueSetProperty[_]]}.""".stripMargin - new ExampleContainer {
      object Prop extends Property("testProperty") with Identifiable {
        def validate(messages: Messages, value: JsValue): Option[JsObject] =
          toType[String](value)
            .right.map {
              case "something" => None
              case _ => Some(obj("validation" -> "error"))
            }
            .left.map(Option.apply)
            .merge
        val extraJson = None
        def determineIdentityOf(value: JsValue) = value.as[String]
      }

      val setProperty: ValueSetProperty[Prop.type] = Prop.*
    }.withSpecification { code =>
      val setProperty = code.setProperty

      def validate(json: Option[JsValue]) = inApp {
        setProperty.validate(messages, json)
      }

      "Validation will not fail if no value is given." - {
        validate(None) is None
      }

      "Validation will fail when the value is not an array" - example {
        validate(Some(obj())) is Some(
          obj(
            "id" -> "testProperty",
            "error" -> "invalidType"
          )
        )
      }

      """|Validation will fail if any of the provided values do not pass
         |the property's validation""".stripMargin - example {
        validate(Some(arr("something", "nothing", "other"))) is Some(
          obj(
            "id" -> "testProperty",
            "errors" -> obj(
              "1" -> obj("validation" -> "error"),
              "2" -> obj("validation" -> "error")
            )
          )
        )
      }

      "Validation will fail if any of the provided values has the same identity" - example {
        validate(Some(arr("something", "something"))) is Some(
          obj(
            "id" -> "testProperty",
            "errors" -> obj(
              "1" -> obj(
                "message" -> "The value `something` is present more than once",
                "messageKey" -> "duplicateValue"
              )
            )
          )
        )
      }

      "It will also add extra information to the `toJson` method" - example {
        val result = setProperty.toJson
        result is obj(
          "id" -> "testProperty",
          "set" -> true,
          "nonEmpty" -> false
        )
      }
    }

    s"""|##`Confidential`
        |
        |The `Confidential` method marks the property as confidential. This means 
        |it will only be accessabile using the private API. This is wrapped around 
        |the property instead of added as a method because it's not dependend on the 
        |type of property.""".stripMargin - new ExampleContainer {
      val prop = Confidential(TestProperty)
    }.withSpecification { code =>

      val prop = code.prop

      "As a counter example: `generated` can not be called on an optional property." - {
        TestProperty.?.getClass.getMethod("generated") must throwA[NoSuchMethodException]
      }

      "It's `confidential` property is set to true" - {
        prop.confidential is true
      }

      "It will also add extra information to the `toJson` method" - example {
        val result = prop.toJson
        result is obj(
          "id" -> "testProperty",
          "confidential" -> true
        )
      }
    }
  }
}