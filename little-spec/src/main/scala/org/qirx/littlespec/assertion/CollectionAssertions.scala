package org.qirx.littlespec.assertion

import scala.collection.Iterable
import org.qirx.littlespec.Assertion
import scala.reflect.ClassTag
import org.qirx.littlespec.Fragment

trait CollectionAssertions {
  def contain[T: ClassTag](matcher: PartialFunction[Any, Unit]): Assertion[T] =
    new Assertion[T] {
      def assert(s: => T): Either[String, Fragment.Body] =
        Left("not implemented CollectionAssertions.contain")
    }
}