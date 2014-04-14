package org.qirx.littlespec.assertion

import scala.reflect.ClassTag
import org.qirx.littlespec.Assertion
import org.qirx.littlespec.Fragment

trait ThrowingAssertions { self: StaticAssertions =>

  def throwA[E <: Throwable: ClassTag]: ExceptionMatcher[E] =
    new ExceptionMatcher[E]
  def throwAn[E <: Throwable: ClassTag]: ExceptionMatcher[E] =
    throwA[E]

  class ExceptionMatcher[E: ClassTag] extends Assertion[Any] {

    def assert(code: => Any) =
      runCode(code, _ => success)

    def like(handler: E => Fragment.Body): Assertion[Any] =
      new Assertion[Any] {
        def assert(code: => Any) =
          runCode(code, handler)
      }

    private def runCode(code: => Any, handler: E => Fragment.Body) =
      try {
        code
        val c = implicitly[ClassTag[E]]
        Left("Expected " + c.runtimeClass.getSimpleName)
      } catch {
        case e: E => Right(handler(e))
      }
  }
}