package org.qirx.cms.machinery

import org.qirx.cms.machinery.TypeSet.ToType
import scala.language.higherKinds

trait Parts[Elem[_]]

object Parts {
  def apply[Types <: TypeSet](
    implicit result: ToType[Types]): Parts[result.Out] = null

  implicit def fromProgramRunner[Types <: TypeSet](
    implicit runner: ProgramRunner[Types],
    result: ToType[Types]): Parts[result.Out] = null
}