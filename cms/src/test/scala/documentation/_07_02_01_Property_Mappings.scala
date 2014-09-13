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

class _07_02_01_Property_Mappings extends Specification {

  val name = "[name]"

  """|These are the specifications of the different property mapping
     |instances that are provided implicitly""".stripMargin - {

    wrappedMappings[OptionalValueProperty[Custom]]
    wrappedMappings[GeneratedValueProperty[Custom]]
    wrappedMappings[ValueSetProperty[Custom]]

    mappings[ConfidentialProperty[Custom]](
      mappings = Seq.empty: _*
    )

    mappings[Identifiable](
      mappings = obj(name -> obj("type" -> "string", "index" -> "not_analyzed"))
    )

    mappings[Label](
      mappings = obj(name -> obj("type" -> "string"))
    )

    mappings[Date](
      mappings = obj(name -> obj("type" -> "date", "format" -> "date_time_no_millis"))
    )

    val name_text = name + "_text"

    mappings[RichContent](
      mappings = Seq(
        obj(name -> obj("enabled" -> false)),
        obj(name_text -> obj("type" -> "string"))
      ): _*
    )

    providedMappings[Tag, Identifiable]
  }

  def providedMappings[T1, T2](
    implicit classTag1: ClassTag[T1], classTag2: ClassTag[T2],
    information1: Mappings[T1],
    information2: Mappings[T2]) = {

    val name1 = classTag1.runtimeClass.getSimpleName
    val name2 = classTag2.runtimeClass.getSimpleName

    s"$name1 is provided by $name2" - {
      information1 is information2
    }
  }

  trait Custom extends PropertyMetadata with Identifiable with GeneratableValue
  object Custom {
    implicit val customMappings = new Mappings[Custom] {
      def mappings(propertyName: String) = Seq(obj(propertyName -> "custom"))
    }
  }

  def wrappedMappings[T](
    implicit classTag: ClassTag[T],
    provided: Mappings[T]) =
    classTag.runtimeClass.getSimpleName - {

      val mapping = obj(name -> "custom")

      val mappings = provided.mappings(name)

      "Provide the wrapped mappings" - {
        mappings is Seq(mapping)
      }
    }

  def mappings[T](mappings: JsObject*)(
    implicit classTag: ClassTag[T],
    provided: Mappings[T]) =
    classTag.runtimeClass.getSimpleName - {

      val providedMappings = provided.mappings(name)
      val mappingsAsString = Json prettyPrint mappings.fold(obj())(_ ++ _)

      s"Provide the correct mappings\n$mappingsAsString" - {
        providedMappings is mappings
      }
    }
}
