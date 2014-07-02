package org.qirx.cms.construction

import org.qirx.cms.machinery.Program
import scala.language.higherKinds 

sealed trait System[T]

case class BranchAction[Left[_], Right[_], A, B](
  left: Program[Left, A],
  right: Program[Right, B],
  stayLeft: A => Boolean) extends System[A]

case class Return[ReturnType](result: ReturnType) extends System[ReturnType]

trait DirectAction[ReturnType] extends System[ReturnType] {
  def result:ReturnType
}