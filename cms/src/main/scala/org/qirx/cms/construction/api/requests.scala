package org.qirx.cms.construction.api

import org.qirx.cms.construction.DirectAction

case class GetFieldSetFromQueryString(queryString: Map[String, Seq[String]]) extends DirectAction[Set[String]]
case class GetNextSegment(path: Seq[String]) extends DirectAction[Option[(String, Seq[String])]]
