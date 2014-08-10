package org.qirx.cms.metadata

import play.api.libs.json.JsObject
import java.text.Normalizer

class DefaultDocumentIdGenerator(idField:String) extends DocumentIdGenerator {
  def generateFor(value: JsObject): String = {
    val stringValue = (value \ idField).as[String]
    val normalizedValue =
      Normalizer.normalize(stringValue, Normalizer.Form.NFD)
    val alphaNumericWithSpaces =
      normalizedValue.replaceAll("[^a-zA-Z0-9 _-]", "")

    val lowerCase = alphaNumericWithSpaces.toLowerCase

    lowerCase.replaceAll(" ", "_")
  }

  val WithUniqueId = """(.+)-(\d+)""".r

  def makeUnique(id: String): String =
    id match {
      case WithUniqueId(originalId, counter) =>
        originalId + "-" + (counter.toInt + 1)
      case id => id + "-1"
    }
}