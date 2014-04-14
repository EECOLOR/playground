package org.qirx.littlespec.assertion

import org.qirx.littlespec.Assertion
import org.qirx.littlespec.Fragment

trait TypeAssertions {
  def beAnInstanceOf[T] =
    new Assertion[Any] {
      def assert(s: => Any): Either[String, Fragment.Body] =
        Left("beAnInstanceOf unimplemented")
    }
}