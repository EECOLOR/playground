package org.qirx.cms.machinery

import org.qirx.cms.construction.Branch
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait ExecutionTools {

  type FutureSeq[T] = Future[Seq[T]]
  implicit def futureSeqChainer(implicit ec: ExecutionContext) =
    new Chainer[FutureSeq] {
      def chain[A, B](fa: FutureSeq[A], to: A => FutureSeq[B]) = {
        fa.flatMap { list =>
          Future.sequence(list.map(to)).map(_.flatten)
        }
      }
    }
  implicit val futureSeqFactory =
    new Factory[FutureSeq] {
      def create[A](a: A) = Future.successful(Seq(a))
    }

  implicit val futureFactory =
    new Factory[Future] {
      def create[A](a: A) = Future.successful(a)
    }
  implicit def futureChainer(implicit ec: ExecutionContext) =
    new Chainer[Future] {
      def chain[A, B](f: Future[A], to: A => Future[B]) = f flatMap to
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