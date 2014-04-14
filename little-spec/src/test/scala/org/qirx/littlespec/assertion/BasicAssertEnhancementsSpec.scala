package org.qirx.littlespec.assertion

import org.qirx.littlespec.Specification
import org.qirx.littlespec.Fragment.ThrowableFailure
import org.qirx.littlespec.Assertion
import org.qirx.littlespec.Fragment

object BasicAssertEnhancementsSpec extends Specification {

  "BasicAssertEnhancements should" - {

    "have an 'is' enhancement" - {

      "that throws a failure exception when the values are not equal" - {
        (1 is 2) must throwA[ThrowableFailure]
      }

      "that returns success when the values are equal" - {
        (1 is 1)
      }
    }

    "have an 'isLike' enhancement" - {

      "that throws a failure exception when the partial function does is not defined" - {
        (1 isLike { case 2 => success }) must throwA[ThrowableFailure]
      }

      "that returns the result of the partial function if it's defined" - {
        (1 isLike { case 1 => todo }) is todo
      }
    }

    "have a 'must' enhancement that accepts Assertions" - {

      "that throws a failure exception when an string is returned" - {
        val assertion =
          new Assertion[String] {
            def assert(s: => String): Either[String, Fragment.Body] =
              Left(s + " - failure")
          }

        ("test" must assertion) must throwA[ThrowableFailure].like {
          case ThrowableFailure(message) => message is "test - failure"
          case _ => failure("no exception was thrown")
        }
      }

      "that returns the body if that is returned" - {
        val assertion =
          new Assertion[String] {
            def assert(s: => String): Either[String, Fragment.Body] =
              Right(success)
          }
        ("test" must assertion) is success
      }
    }
  }

}