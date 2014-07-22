package org.qirx.cms.machinery

import scala.language.higherKinds

trait ProgramRunner[ProgramParts[_], ProgramResult[_]] extends Parts[ProgramParts] {
  
  def runner:ProgramParts ~> ProgramResult
  
  implicit def monad:Free.Monad[ProgramResult]
  
  def run[Result](program:Program[ProgramParts, Result]):ProgramResult[Result] = 
    program.foldMap(runner)
}

object ProgramRunner {
  def fromRunner[ProgramParts[_], ProgramResult[_] : Free.Monad, T[_]](r:T ~> ProgramResult)(
      implicit toParts: T ~> ProgramResult => ProgramParts ~> ProgramResult
  ) = new ProgramRunner[ProgramParts, ProgramResult] {
    def runner = r
    def monad = Free.Monad[ProgramResult]
  }
}