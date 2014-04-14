package org.qirx.littlespec.assertion

import org.qirx.littlespec.Specification
import org.qirx.littlespec.Fragment
import testUtils.beAFailure

object StaticAssertionsSpec extends Specification {

  "StaticAssertions should provide" - {

    "an alias for todo" - {
      todo is Fragment.Body.Todo
    }

    "an alias for success" - {
      success is Fragment.Body.Success
    }

    "an alias for pending" - {
      pending("test") is Fragment.Body.Pending("test")
    }

    "a shortcut for failure" - {
      failure("test") must beAFailure.like {
        _.message is "test"
      }
    }
  }

}