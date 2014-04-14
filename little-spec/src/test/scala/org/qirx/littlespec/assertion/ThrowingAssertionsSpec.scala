package org.qirx.littlespec.assertion

import org.qirx.littlespec.Specification

object ThrowingAssertionsSpec extends Specification {

  "ThrowingAsserts should" - {

    "have a throwA and throwAn assertion" - {

      "that returns some message when no exception was thrown" - {
        val expected = Left("Expected 'CustomException' but no exception was thrown")
        throwA[CustomException].assert() is expected
        throwAn[CustomException].assert() is expected
      }

      "that returns success if the exception is thrown" - {
        def code = throw CustomException("test")
        val expected = Right(success)
        throwA[CustomException].assert(code) is Right(success)
        throwAn[CustomException].assert(code) is Right(success)
      }

      "that throws the exception if it's of another type" - {
        def code = throw new RuntimeException
        val expected = throwA[RuntimeException]
        throwA[CustomException].assert(code) must expected
        throwAn[CustomException].assert(code) must expected

      }

      "that can be enhanced with a function passed to the like method which" - {

        "returns some message when no exception was thrown" - {
          val expected = Left("Expected 'CustomException' but no exception was thrown")

          throwA[CustomException].like {
            case CustomException(message) =>
          }.assert() is expected

          throwAn[CustomException].like {
            case CustomException(message) =>
          }.assert() is expected
        }

        "calls the method if the exception was thrown" - {
          def code = throw CustomException("test")
          val expected = Right(pending("test"))

          throwA[CustomException].like {
            case CustomException(message) => pending(message)
          }.assert(code) is expected

          throwAn[CustomException].like {
            case CustomException(message) => pending(message)
          }.assert(code) is expected
        }
      }

    }
  }
}

case class CustomException(message: String) extends Throwable