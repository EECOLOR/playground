package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.elasticsearch.PropertyMetadataIndexInformation
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

class _07_02_01_Property_Metadata_Index_Information extends Specification {

  val name = "[name]"

  """|These are the specifications of the different property metadata index information
     |instances that are provided implicitly""".stripMargin - {

    wrapped[OptionalValueProperty[Custom]]
    wrapped[GeneratedValueProperty[Custom]]
    wrapped[ValueSetProperty[Custom]]

    noTransformation[ConfidentialProperty[Custom]](
      mappings = Seq.empty: _*
    )

    noTransformation[Identifiable](
      mappings = obj(name -> obj("type" -> "string", "index" -> "not_analyzed"))
    )

    noTransformation[Label](
      mappings = obj(name -> obj("type" -> "string"))
    )

    noTransformation[Date](
      mappings = obj(name -> obj("type" -> "date", "format" -> "date_time_no_millis"))
    )

    val name_text = name + "_text"
    val value = arr("one ", obj("text" -> "two"))
    val text = "one two"

    withTransformation[RichContent](
      mappings = Seq(
        obj(name -> obj("enabled" -> false)),
        obj(name_text -> obj("type" -> "string"))
      ),
      document = obj(name -> value),
      transformationResult = obj(
        name -> value,
        name_text -> text
      )
    )
    
    provided[Tag, Identifiable]
  }

  def provided[T1, T2](
    implicit classTag1: ClassTag[T1], classTag2: ClassTag[T2],
    information1: PropertyMetadataIndexInformation[T1],
    information2: PropertyMetadataIndexInformation[T2]) = {
    
    val name1 = classTag1.runtimeClass.getSimpleName
    val name2 = classTag2.runtimeClass.getSimpleName

    s"$name1 is provided by $name2" - {
      information1 is information2
    }
  }

  def withTransformation[T](
    mappings: Seq[JsObject],
    document: JsObject,
    transformationResult: JsObject)(
      implicit classTag: ClassTag[T],
      information: PropertyMetadataIndexInformation[T]) =
    classTag.runtimeClass.getSimpleName - {

      val providedMappings = information.mappings(name)
      val mappingsAsString = Json prettyPrint mappings.fold(obj())(_ ++ _)

      s"Provide the correct mappings\n$mappingsAsString" - {
        providedMappings is mappings
      }

      "Perform the correct transformation" - {
        val transformed = information.transform(name, document)
        transformed is transformationResult
      }
    }

  trait Custom extends PropertyMetadata with Identifiable with GeneratableValue
  implicit val info = new PropertyMetadataIndexInformation[Custom] {
    def mappings(propertyName: String) = Seq(obj(propertyName -> "custom"))
    def transform(propertyName: String, document: JsObject) = obj("custom" -> propertyName)
  }

  def wrapped[T](
    implicit classTag: ClassTag[T],
    information: PropertyMetadataIndexInformation[T]) =
    classTag.runtimeClass.getSimpleName - {

      val mapping = obj(name -> "custom")
      val transformationResult = obj("custom" -> name)

      val mappings = information.mappings(name)

      "Provide the wrapped mappings" - {
        mappings is Seq(mapping)
      }

      "Perform the wrapped transformation" - {
        val document = obj(name -> "test")
        val transformed = information.transform(name, document)
        transformed is transformationResult
      }
    }

  def noTransformation[T](mappings: JsObject*)(
    implicit classTag: ClassTag[T],
    information: PropertyMetadataIndexInformation[T]) =
    classTag.runtimeClass.getSimpleName - {

      val providedMappings = information.mappings(name)
      val mappingsAsString = Json prettyPrint mappings.fold(obj())(_ ++ _)

      s"Provide the correct mappings\n$mappingsAsString" - {
        providedMappings is mappings
      }

      "Perform no transformation" - {
        val document = obj(name -> "test")
        val transformed = information.transform(name, document)
        transformed is document
      }
    }
}
