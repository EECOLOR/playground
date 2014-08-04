package org.qirx.cms.machinery

import play.api.mvc.Result
import org.qirx.cms.construction.Branch
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait ExecutionTools {
  implicit val executionContext:ExecutionContext
  
  object BranchToFuture extends (Branch[Result]#Instance ~> FutureResultBranch) {
    def transform[x] = x => FutureResultBranch(Future successful x)
  }

  object IdToBranch extends (Id ~> Branch[Result]#Instance) {
    def transform[x] = x => Branch[Result].Instance(Left(x))
  }

  object FutureToFutureBranch extends (Future ~> FutureResultBranch) {
    def transform[x] = x => FutureResultBranch(x map IdToBranch.apply)
  }
  
  object IdToFuture extends (Id ~> Future) {
    def transform[x] = x => Future.successful(x)
  }
}