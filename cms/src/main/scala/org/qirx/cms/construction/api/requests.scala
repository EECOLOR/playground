package org.qirx.cms.construction.api

import org.qirx.cms.construction.DirectAction

case class GetFieldSetFromQueryString(queryString: Map[String, Seq[String]]) extends DirectAction[Set[String]] {
  val result = {
    val fields = queryString.get("fields")
    fields.toSet.flatten.flatMap(_.split(","))
  }
}
case class GetNextSegment(path: Seq[String]) extends DirectAction[Option[(String, Seq[String])]] {
  val result = path.headOption.map(_ -> path.tail)
}
