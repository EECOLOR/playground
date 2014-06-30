package org.qirx.cms.machinery

import scala.language.higherKinds

case class Program[F[_], A](free: Free[F, A]) {

  import Coproduct.|

  def flatMap[G[_], B](f: A => Program[G, B])(
    implicit c: F | G): Program[c.Out, B] =
    Program(free
      .mapSuspension(c.transformLeft)
      .flatMap(f andThen (_.free.mapSuspension(c.transformRight))))

  def mapSuspension[G[_]](implicit transform: F ~> G): Program[G, A] =
    Program(free.mapSuspension[G])

  def map[B](f: A => B): Program[F, B] =
    Program(free.map(f))

  def run[G[_]: Free.Monad](transform: F ~> G): G[A] =
    free.foldMap(transform)

  // pattern match support
  def withFilter(f: Any => Boolean): Program[F, A] =
    this
}

object Program {

  def apply[F[_], A](fa: F[A]): Program[F, A] =
    Program(Free(fa))
}