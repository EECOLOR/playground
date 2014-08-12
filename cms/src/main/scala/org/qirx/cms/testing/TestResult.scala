package org.qirx.cms

import scala.language.higherKinds
import org.qirx.cms.machinery.Id
import scala.language.existentials

package object testing {

  sealed trait TestResult[Typeclass[_]] {
    def fold[S](onSuccess: => S, onFailure: TestFailure[_, Typeclass] => S): S
  }

  case class TestFailure[T, Typeclass[_]](value: T, expectedValue: T)(
    implicit val typeclass: Typeclass[T]) extends TestResult[Typeclass] {

    def fold[S](onSuccess: => S, onFailure: TestFailure[_, Typeclass] => S): S =
      onFailure(this)
  }

  def TestSuccess[Typeclass[_]] =
    new TestResult[Typeclass] {
      def fold[S](onSuccess: => S, onFailure: TestFailure[_, Typeclass] => S): S =
        onSuccess
    }
}
