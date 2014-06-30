package org.qirx.cms.machinery

import scala.language.higherKinds

trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Apply(a) => f(a)
      case FlatMap(i, k) =>
        FlatMap(i, k andThen (_ flatMap f))
    }

  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Apply(f(a)))

  def mapSuspension[G[_]](implicit transform: F ~> G): Free[G, A] =
    this match {
      case Apply(a) => Apply(a)
      case FlatMap(i, k) =>
        FlatMap(transform(i), k andThen (_.mapSuspension))
    }

  def foldMap[G[_]: Free.Monad](transform: F ~> G): G[A] = {
    val G = Free.Monad[G]
    this match {
      case Apply(a) => G(a)
      case FlatMap(fa, f) =>
        G.flatMap(transform(fa), f andThen (_ foldMap transform))
    }
  }
}

object Free {

  def apply[F[_], A](f: F[A]): Free[F, A] = FlatMap(f, Apply(_: A))

  type Id[x] = x

  trait Monad[F[_]] {
    def apply[A](a: A): F[A]
    def flatMap[A, B](a: F[A], f: A => F[B]): F[B]
  }

  object Monad {
    def apply[F[_]: Monad]: Monad[F] = implicitly

    type Id[x] = x

    implicit val idMonad =
      new Monad[Id] {
        def apply[A](a: A) = a
        def flatMap[A, B](a: Id[A], f: A => Id[B]) = f(a)
      }

    implicit def freeMonad[F[_]] =
      new Monad[({ type T[x] = Free[F, x] })#T] {
        def apply[A](a: A) = Apply(a)
        def flatMap[A, B](fa: Free[F, A], f: A => Free[F, B]) = fa flatMap f
      }
  }
}

case class Apply[F[_], A](value: A) extends Free[F, A]

case class FlatMap[F[_], A, B](input: F[A], f: A => Free[F, B]) extends Free[F, B]
