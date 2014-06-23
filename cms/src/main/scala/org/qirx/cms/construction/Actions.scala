package org.qirx.cms.construction

sealed trait EssentialAction[ReturnType]

case class Return[ReturnType](result: ReturnType) extends DirectAction[ReturnType]

case class BranchAction[A, B](stayLeft: Boolean, left: A, right: B) extends Action[A]

trait DirectAction[ReturnType] extends Action[ReturnType] {
  println("directAction does not have the correct return type yet, fix once you know how to mix types with composition of Free")
  def result: ReturnType
}

trait Action[ReturnType] extends EssentialAction[ReturnType]