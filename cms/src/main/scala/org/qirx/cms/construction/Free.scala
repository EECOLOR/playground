package org.qirx.cms.construction

import scala.language.higherKinds

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    this match {
      case Apply(a) => f(a)
      case Bind(i, k) =>
        Bind(i, k andThen (_ flatMap f))
    }
  def map[B](f: A => B): Free[F, B] =
    flatMap(a => Apply(f(a)))

  //for pattern matching
  def withFilter(f: Any => Boolean): Free[F, A] = this
}

case class Apply[F[_], A](t: A) extends Free[F, A]
case class Bind[F[_], A, B](i: F[A], k: A => Free[F, B]) extends Free[F, B]

