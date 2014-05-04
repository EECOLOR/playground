package org.qirx.littlespec.assertion

import org.qirx.littlespec.Specification
import org.qirx.littlespec.Assertion
import org.qirx.littlespec.Fragment
import testUtils.beAFailure
import testUtils.beAFailureWithMessage

object BasicAssertEnhancementsSpec extends Specification {

  "BasicAssertEnhancements should" - {

    "have an 'is' enhancement" - {

      "that throws a failure exception when the values are not equal" - {
        (1 is 2) must beAFailure
      }

      "that returns success when the values are equal" - {
        (1 is 1)
      }
    }

    "have an 'isLike' enhancement" - {

      "that throws a failure exception when the partial function does is not defined" - {
        (1 isLike { case 2 => success }) must beAFailure
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

        ("test" must assertion) must beAFailureWithMessage("test - failure")
      }

      "that returns the body if that is returned" - {
        val assertion =
          new Assertion[String] {
            def assert(s: => String): Either[String, Fragment.Body] =
              Right(success)
          }
        ("test" must assertion) is success
      }

      "that accepts different assertion types as long as there is an implicit conversion" - {
        val assertion =
          new Assertion[Seq[Char]] {
            def assert(s: => Seq[Char]): Either[String, Fragment.Body] =
              Right(success)
          }

        ("test" must assertion) is success
      }
    }

    "have a 'withMessage' enhancement for fragment bodies" - {
      val message1 = "test1"
      val message2 = "test2"
      def body: Fragment.Body = failure(message1)

      "that allows you to change the message" - {
        def result = body withMessage (_ + " " + message2)
        result must beAFailureWithMessage(message1 + " " + message2)
      }

      "that allows you to replace the message" - {
        def result = body withMessage message2
        result must beAFailureWithMessage(message2)
      }
    }
  }
}