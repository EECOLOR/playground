package org.qirx.cms.machinery

import play.api.mvc.Result
import org.qirx.cms.construction.Branch
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait ExecutionTools {
  implicit val executionContext: ExecutionContext

  type FutureSeq[T] = Future[Seq[T]]
  implicit def monad = new Free.Monad[FutureSeq] {
    def apply[A](a: A) = Future.successful(Seq(a))
    def flatMap[A, B](fa: FutureSeq[A], f: A => FutureSeq[B]) = {
      fa.flatMap { list =>
        Future.sequence(list.map(f)).map(_.flatten)
      }
    }
  }

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

  object SeqToFutureSeq extends (Seq ~> FutureSeq) {
    def transform[x] = x => Future.successful(x)
  }

  object FutureToFutureSeq extends (Future ~> FutureSeq) {
    def transform[x] = x => x.map(x => Seq(x))
  }
}