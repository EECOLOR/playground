package org.qirx.cms.construction

import org.qirx.cms.machinery.Program

sealed trait System[T]

case class Return[ReturnType](result: ReturnType) extends System[ReturnType]

trait DirectAction[ReturnType] extends System[ReturnType] {
  def result:ReturnType
}
