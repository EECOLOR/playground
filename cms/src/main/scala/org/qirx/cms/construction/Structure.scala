package org.qirx.cms.construction

import org.qirx.cms.machinery.Program
import scala.language.higherKinds 

trait Structure[T]
case class BranchAction[Left[_], Right[_], A, B](
  left: Program[Left, A],
  right: Program[Right, B],
  stayLeft: A => Boolean) extends Structure[A]