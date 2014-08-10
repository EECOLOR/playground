package org.qirx.cms.execution

import org.qirx.cms.construction.DirectAction
import org.qirx.cms.construction.Return
import org.qirx.cms.construction.System
import org.qirx.cms.machinery.Id
import org.qirx.cms.machinery.~>

object SystemToId extends (System ~> Id) {

  def transform[x] = {
    case Return(result) => result
    case directAction:DirectAction[_] => directAction.result
  }
}