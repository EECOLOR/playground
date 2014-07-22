package org.qirx.cms.machinery

import scala.language.higherKinds
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Promise
import org.qirx.cms.construction.Branch
import play.api.mvc.Result

case class FutureResultBranch[x](value: Future[Branch[Result]#Instance[x]])

object FutureResultBranch {
  implicit def monad(implicit ec:ExecutionContext) = new Free.Monad[FutureResultBranch] {
    def apply[A](a:A) = FutureResultBranch(Future.successful(Branch[Result].Instance(Left(a))))
    def flatMap[A, B](fa:FutureResultBranch[A], f:A => FutureResultBranch[B]) = {
      FutureResultBranch(
        fa.value.flatMap { branch =>
          branch.value match {
            case Left(a) => f(a).value
            case Right(b) => Future.successful(Branch[Result].Instance[B](Right(b)))
          }
        }
      )
    } 
      
  }
}