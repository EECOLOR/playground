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

  trait Custom extends PropertyMetadata with Identifiable with GeneratableValue

  s"""|For the built-in types a mappings are provided.
      |
      |These are the default mappings:""".stripMargin - {

    implicit val customMappings = new Mappings[Custom] {
      def mappings(propertyName: String) = Seq(obj(propertyName -> "custom"))
    }

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

  def wrappedMappings[T](
    implicit classTag: ClassTag[T],
    provided: Mappings[T]) =
    s"`${classTag.runtimeClass.getSimpleName}[Custom]` is delegating to to the `Custom` mapping" - {

      val mapping = obj(name -> "custom")
      provided.mappings(name) is Seq(mapping)
    }

  def mappings[T](mappings: JsObject*)(
    implicit classTag: ClassTag[T],
    provided: Mappings[T]) = {

    val mappingsAsString = Json prettyPrint mappings.fold(obj())(_ ++ _)

    s"""|`${classTag.runtimeClass.getSimpleName}` is mapped as
        |```json
        |$mappingsAsString
        |```""".stripMargin - {
      provided.mappings(name) is mappings
    }
  }

  def providedMappings[T1, T2](
    implicit classTag1: ClassTag[T1], classTag2: ClassTag[T2],
    provided1: Mappings[T1],
    provided2: Mappings[T2]) = {

    val name1 = classTag1.runtimeClass.getSimpleName
    val name2 = classTag2.runtimeClass.getSimpleName

    s"`$name1` is provided by `$name2`" - {
      provided1 is provided2
    }
  }
}
