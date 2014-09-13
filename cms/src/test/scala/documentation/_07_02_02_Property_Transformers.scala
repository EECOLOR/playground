package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.metadata.properties.Label
import play.api.libs.json.Json.obj
import play.api.libs.json.JsObject
import scala.reflect.ClassTag
import org.qirx.cms.metadata.properties.Date
import org.qirx.cms.metadata.dsl.Identifiable
import play.api.libs.json.Json
import play.api.libs.json.Json.arr
import org.qirx.cms.metadata.dsl.OptionalValueProperty
import org.qirx.cms.metadata.dsl.Identifiable
import org.qirx.cms.metadata.PropertyMetadata
import org.qirx.cms.metadata.dsl.GeneratedValueProperty
import org.qirx.cms.metadata.dsl.GeneratableValue
import org.qirx.cms.metadata.dsl.ValueSetProperty
import org.qirx.cms.metadata.dsl.ConfidentialProperty
import play.api.libs.json.JsValue
import org.qirx.cms.metadata.properties.RichContent
import org.qirx.cms.metadata.properties.Tag
import org.qirx.cms.elasticsearch.index.PropertyTransformer
import org.qirx.cms.elasticsearch.index.PropertyMappings
import org.qirx.cms.elasticsearch.index.Mappings
import org.qirx.cms.elasticsearch.index.Transformer

class _07_02_02_Property_Transformers extends Specification {

  val name = "[name]"

  """|These are the specifications of the different property transformation
     |instances that are provided implicitly""".stripMargin - {

    "Identity transformation for any type" - {
      val transformer = implicitly[Transformer[Any]]
      val testDocument = obj("test" -> "two")
      val resultDocument = transformer.transform(name, testDocument)

      testDocument eq resultDocument is true
    }

    wrappedTransformation[OptionalValueProperty[Custom]]
    wrappedTransformation[GeneratedValueProperty[Custom]]
    wrappedTransformation[ValueSetProperty[Custom]]

    val name_text = name + "_text"
    val value = arr("one ", obj("text" -> "two"))
    val text = "one two"

    transformation[RichContent](
      document = obj(name -> value),
      transformationResult = obj(
        name -> value,
        name_text -> text
      )
    )
  }

  def transformation[T](
    document: JsObject,
    transformationResult: JsObject)(
      implicit classTag: ClassTag[T],
      provided: Transformer[T]) =
    classTag.runtimeClass.getSimpleName - {

      "Perform the correct transformation" - {
        val transformed = provided.transform(name, document)
        transformed is transformationResult
      }
    }

  trait Custom extends PropertyMetadata with Identifiable with GeneratableValue
  object Custom {
    implicit val customTransformer = new Transformer[Custom] {
      def transform(propertyName: String, document: JsObject) = obj("custom" -> propertyName)
    }
  }

  def wrappedTransformation[T](
    implicit classTag: ClassTag[T],
    provided: Transformer[T]) =
    classTag.runtimeClass.getSimpleName - {

      val transformationResult = obj("custom" -> name)

      "Perform the wrapped transformation" - {
        val document = obj(name -> "test")
        val transformed = provided.transform(name, document)
        transformed is transformationResult
      }
    }
}
