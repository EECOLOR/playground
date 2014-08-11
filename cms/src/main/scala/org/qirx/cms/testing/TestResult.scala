package org.qirx.cms.testing

import scala.language.higherKinds
import org.qirx.cms.machinery.Id
import scala.language.existentials

sealed trait TestResult[T, Typeclass[_]] {
  def fold[S](onSuccess: => S, onFailure: TestFailure[T, Typeclass] => S):S
}

case class TestFailure[T, Typeclass[_]](value:T, expectedValue:T)(
    implicit val typeclass:Typeclass[T]) extends TestResult[T, Typeclass] {
  def fold[S](onSuccess: => S, onFailure: TestFailure[T, Typeclass] => S):S =
    onFailure(this)
}

case object TestSuccess extends TestResult[Nothing, T forSome {type T[_]}] {
  def fold[S](onSuccess: => S, onFailure: TestFailure[Nothing, T forSome {type T[_]}] => S):S =
    onSuccess
}