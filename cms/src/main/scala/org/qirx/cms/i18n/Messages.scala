package org.qirx.cms.i18n

import play.api.i18n.{Messages => PlayMessages}

class Messages(prefix:Option[String] = None) {
  
  def apply(key: String, arguments: String*): String = 
    PlayMessages(prefix.map(_ + "." + key).toSeq :+ key, arguments: _*)
    
  def withPrefix(newPrefix: String): Messages = 
    new Messages(prefix map (_ + "." + newPrefix) orElse Some(newPrefix))
}

object Messages extends Messages(prefix = None)