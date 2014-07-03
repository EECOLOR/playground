package org.qirx.cms.machinery

import scala.language.higherKinds

trait ProgramRunner[ProgramParts[_], ProgramResult[_]] extends Parts[ProgramParts] {
  
  def runner:ProgramParts ~> ProgramResult
  
  implicit def monad:Free.Monad[ProgramResult]
  
  def run[Result](program:Program[ProgramParts, Result]):ProgramResult[Result] = 
    program.foldMap(runner)
}