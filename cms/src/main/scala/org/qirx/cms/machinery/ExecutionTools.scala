package org.qirx.cms.machinery

import play.api.mvc.Result
import org.qirx.cms.construction.Branch
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait ExecutionTools {

  type FutureSeq[T] = Future[Seq[T]]
  implicit def futureSeqMonad(implicit ec: ExecutionContext) =
    new Free.Monad[FutureSeq] {
      def apply[A](a: A) = Future.successful(Seq(a))
      def flatMap[A, B](fa: FutureSeq[A], f: A => FutureSeq[B]) = {
        fa.flatMap { list =>
          Future.sequence(list.map(f)).map(_.flatten)
        }
      }
    }

  implicit def futureMonad(implicit ec: ExecutionContext) =
    new Free.Monad[Future] {
      def apply[A](a: A) = Future.successful(a)
      def flatMap[A, B](fa: Future[A], f: A => Future[B]) =
        fa flatMap f
    }

  object IdToFuture extends (Id ~> Future) {
    def transform[x] = x => Future.successful(x)
  }

  object SeqToFutureSeq extends (Seq ~> FutureSeq) {
    def transform[x] = x => Future.successful(x)
  }

  def FutureToFutureSeq(implicit ec: ExecutionContext) =
    new (Future ~> FutureSeq) {
      def transform[x] = x => x.map(x => Seq(x))
    }
}

object ExecutionTools extends ExecutionTools