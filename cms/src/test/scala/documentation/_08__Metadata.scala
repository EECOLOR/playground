package documentation

import testUtils.cmsName
import testUtils.moreInformation
import org.qirx.littlespec.Specification
import org.qirx.cms.metadata.DocumentMetadata
import org.qirx.cms.metadata.dsl.Document
import org.qirx.cms.metadata.properties.RichContent
import org.qirx.cms.metadata.properties.RichContentElement
import org.qirx.cms.metadata.dsl.Property
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages

class _08__Metadata extends Specification {
  """|#Metadata
     |
     |Metadata is at the core of the ${cmsName}, there are different kinds of 
     |metadata that operate on different levels.""".stripMargin - {

    val `property metadata` = null

    """|##Document metadata
       |
       |Document metadata provides information about the document. It has a 
       |convenient factory that allows easy creation. If you want to supply 
       |you own document metadata, please look at the source of that class 
       |to get a suggestion on rolling your own.""".stripMargin - example {

      Document(
        id = "The identifier for this type of document",
        idField = """
          The field that will be used to determine the identifier of
          document instances.
        """)(
          "property x" -> `property metadata`,
          "property y" -> `property metadata`
        )

      success
    }

    s"""|##Property metadata
        |
        |Property metadata provides information about a single property. There 
        |are different types of properties provided. It is however possible to 
        |introduce new types. Please look at the source of existing implementations 
        |to get suggestions of creating your own.
        |
        |${moreInformation[_08_01_Property_DSL]} 
        |
        |Note that the following structure allows you to reuse metadata under a 
        |different identifier. This can be useful when you need to supply other 
        |constructor arguments""".stripMargin - example {

      class CustomProperty(id: String) extends Property(id) {

        def validate(messages: Messages, value: JsValue): Option[JsObject] = ???
        def extraJson: Option[JsObject] = ???
      }

      object CustomProperty extends CustomProperty("customRichContent")
      
      success
    }

    "This comes in handy if you are using a property metadata type that accepts arguments" - example {
      object CustomRichContent extends RichContent(
        id = "customRichContent",
        allowedElements = Seq(
          RichContentElement("a", Seq("href", "hreflang", "title", "target")),
          RichContentElement("br"), RichContentElement("p", Seq("class", "lang"))
        )
      )
      success
    }

  }
}