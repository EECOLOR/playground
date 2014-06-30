package org.qirx.cms.construction

import org.qirx.cms.machinery.Program
import scala.language.higherKinds

case class Return[ReturnType](result: ReturnType)

case class BranchAction[Left[_], Right[_], A, B](
  left: Program[Left, A],
  right: Program[Right, B],
  stayLeft: A => Boolean) extends Action[A]

trait DirectAction[ReturnType] {
  println("directAction does not have the correct return type yet, fix once you know how to mix types with composition of Free")
  def result: ReturnType
}

trait Action[ReturnType]

case class Choose[A, B, F[_]](value:A)(val f:A => Program[F, B]) extends Action[B]
