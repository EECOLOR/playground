package org.qirx.cms.system

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat

import play.api.libs.json.JsError
import play.api.libs.json.JsString
import play.api.libs.json.JsSuccess
import play.api.libs.json.Reads
import play.api.libs.json.Writes

object IsoDate {
  // Uses ISO 8601 with the following pattern: yyyy-MM-dd'T'HH:mm:ssZZ
  private val jodaDateTimeFormatter = ISODateTimeFormat.dateTimeNoMillis
  
  val reads = Reads {
    case JsString(value) =>
      try JsSuccess(jodaDateTimeFormatter parseDateTime value)
      catch {
        case e: IllegalArgumentException => JsError("Invalid date-time")
      }
    case _ => JsError("Not a string")
  }
  
  val writes = Writes[DateTime] { dateTime =>
    JsString(jodaDateTimeFormatter print dateTime)
  }
}