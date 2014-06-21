package org.qirx.cms.metadata.properties

import org.qirx.cms.metadata.dsl.Property
import play.api.libs.json.JsValue
import play.api.libs.json.JsObject
import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsArray
import play.api.libs.json.JsString
import scala.collection.Set

case class RichContentElement(name: String, attributes: Seq[String] = Seq.empty) {
  override def toString = s"$name[${attributes mkString "|"}]"
}

class RichContent(id: String, protected val allowedElements: Seq[RichContentElement])
  extends Property(id) {

  protected lazy val allowedElementsMap =
    allowedElements.map(e => e.name -> e.attributes.toSet).toMap

  def validate(messages: Messages, value: JsValue): Option[JsObject] =
    toType[JsArray](value)
      .right.map(validateValues(messages, _))
      .left.map(Option.apply)
      .merge

  protected def validateValues(messages: Messages, values: JsArray): Option[JsObject] = {
    val elements = values.as[Seq[JsValue]]
    val errors =
      elements.flatMap {
        case JsString(_) => None
        case value: JsObject => validateObject(messages, value)
        case value => Some(incorrectElement(messages, value))
      }

    if (errors.isEmpty) None
    else Some(errorObj(errors))
  }

  protected def incorrectElement(messages: Messages, value: JsValue) =
    messageObj(messages, "incorrectElement", value.toString)

  protected def validateObject(messages: Messages, value: JsObject): Option[JsObject] = {
    val element = (value \ "element").asOpt[String]
    val text = (value \ "text").asOpt[String]
    val attributes = (value \ "attributes").asOpt[JsObject]
    val children = (value \ "children").asOpt[JsArray]

    val validateChildren = children.map(validateValues(messages, _)).toLeft(())

    val getElement = element.toRight(Some(incorrectElement(messages, value)))

    def getAllowedAttributes(element: String) = allowedElementsMap.get(element)
      .toRight(Some(elementNotAllowed(messages, element)))

    val getAttributes = attributes.map(_.keys).toRight(None)

    def areAttributesAllowed(attributes: Set[String], allowedAttributes: Set[String]) = {
      val disallowed = attributes diff allowedAttributes
      if (disallowed.isEmpty) Right(())
      else Left(Some(attributesNotAllowed(messages, disallowed)))
    }

    import scala.language.implicitConversions
    implicit def rightAssociative[A, B](e: Either[A, B]) = e.right
    
    val result =
      for {
        element <- getElement
        _ <- validateChildren
        allowedAttributes <- getAllowedAttributes(element)
        attributes <- getAttributes
        _ <- areAttributesAllowed(attributes, allowedAttributes)
      } yield None

    result.merge
  }

  protected def elementNotAllowed(messages: Messages, element: String) =
    messageObj(messages, "elementNotAllowed", element)

  protected def attributesNotAllowed(messages: Messages, attributes: Set[String]) =
    messageObj(messages, "attributesNotAllowed", attributes mkString ", ")

}

object RichContent extends RichContent("rich_content",
  allowedElements = Seq(
    RichContentElement("strong"), RichContentElement("em"),
    RichContentElement("ul"), RichContentElement("ol"), RichContentElement("li"),
    RichContentElement("span", Seq("class", "lang]")),
    RichContentElement("a", Seq("href", "hreflang", "title", "target")),
    RichContentElement("br"), RichContentElement("p", Seq("class", "lang"))
  )
) {

  val defaultAllowedElements = allowedElements
}
