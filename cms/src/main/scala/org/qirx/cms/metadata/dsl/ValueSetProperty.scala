package org.qirx.cms.metadata.dsl

import org.qirx.cms.i18n.Messages
import play.api.libs.json.JsArray
import play.api.libs.json.JsObject
import play.api.libs.json.JsValue
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.PropertyMetadata

trait Identifiable { _: PropertyMetadata =>
  private def set(nonEmpty: Boolean) = new ValueSetProperty[this.type](this, nonEmpty)
  //def + = set(nonEmpty = true)
  def * = set(nonEmpty = false)

  def determineIdentityOf(value: JsValue): String
}

class ValueSetProperty[T <: PropertyMetadata with Identifiable](property: T, nonEmpty: Boolean)
  extends WrappedProperty[T](property) with PropertyValidation {

  final val generator = None

  protected val allowEmpty = !nonEmpty

  def validate(messages: Messages, value: Option[JsValue]): Option[JsObject] =
    value match {
      case None =>
        if (allowEmpty) None
        else Some(messageObj(messages, "empty"))
      case Some(value) =>
        toType[Seq[JsValue]](value)
          .right.map(validateSeq(messages, _))
          .left.map(Option.apply)
          .merge
    }

  protected def validateSeq(messages: Messages, values: Seq[JsValue]): Option[JsObject] = {
    val valuesWithIndex = values.zipWithIndex

    val validationErrors = valuesWithIndex.flatMap {
      case (value, index) =>
        property.validate(messages, Some(value)).map(index.toString -> _)
    }
    
    if (validationErrors.isEmpty) checkDuplicateIdentities(messages, valuesWithIndex)
    else Some(errorObj(JsObject(validationErrors)))
  }

  protected def checkDuplicateIdentities(messages: Messages, valuesWithIndex: Seq[(JsValue, Int)]): Option[JsObject] = {
    val identitiesAndErrors = (Set.empty[String], Seq.empty[(String, JsObject)])
    val (_, errors) =
      valuesWithIndex.foldLeft(identitiesAndErrors) {
        case ((identities, errors), (value, index)) =>

          val identity = property determineIdentityOf value
          if (identities contains identity) {
            val error = index.toString -> messageObj(messages, "duplicateValue", "`" + identity + "`")
            (identities, errors :+ error)
          } else (identities + identity, errors)
      }
    if (errors.isEmpty) None
    else Some(errorObj(JsObject(errors)))
  }

  lazy val toJson = property.toJson ++ obj("set" -> true, "nonEmpty" -> nonEmpty)
}
