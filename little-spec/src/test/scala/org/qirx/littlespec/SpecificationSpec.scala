package org.qirx.littlespec

import org.qirx.littlespec.Fragment.ThrowableFailure

object SpecificationSpec extends Specification {

  "Specification should" - {

    "create the correct example" - {

      def example(title: String, code: => Fragment.Body) =
        new DefaultFragment(title, code, new DefaultEventBus)

      "for success" - {
        example("success", success)
          .execute isLike {
            case Success("success") => success
          }
      }

      "for failure" - {
        example("failure title", failure("failure message"))
          .execute is Failure("failure title", "failure message")
      }

      "for todo" - {
        example("todo title", todo)
          .execute is Pending("todo title", "TODO")
      }

      "for unit" - {
        example("unit", {})
          .execute is Pending("unit", "TODO")
      }

      "for exceptions" - {
        val e = new RuntimeException("content")
        example("exception", { throw e; () })
          .execute is UnexpectedFailure("exception", e)
      }
    }

    "have an lt matcher" - {
      "that reports failure if the element is larger" - {
        (2 isLessThan 1) must throwA[ThrowableFailure]
      }
    }

  }

}