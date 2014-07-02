package org.qirx.cms.machinery

import scala.language.higherKinds

trait ProgramRunner[Types <: TypeSet] {
  type ProgramParts[_]
  type ProgramResult[_]
  type Runner = ProgramParts ~> ProgramResult
}