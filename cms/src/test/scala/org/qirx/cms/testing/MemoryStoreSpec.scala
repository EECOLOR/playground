package org.qirx.cms.testing

import org.qirx.littlespec.Specification

import testUtils.PrettyPrint

class MemoryStoreSpec extends Specification {

  "#The memory store should" - {
    import play.api.libs.concurrent.Execution.Implicits.defaultContext

    val storeTester = new StoreTester[PrettyPrint]

    val result = storeTester.test(new MemoryStore)

    result.foreach {
      case (description, result) =>
        result.fold(
          onSuccess = createFragment(description, success),
          onFailure = {
            case testFailure @ TestFailure(value, expectedValue) =>
              val prettyPrint = testFailure.typeclass
              val prettyValue = prettyPrint print value
              val prettyExpectedValue = prettyPrint print expectedValue

              val failureDescription =
                s"""|Expected:
                    |$prettyExpectedValue
                    |Got:
                    |$prettyValue""".stripMargin

              createFragment(description, failure(failureDescription))
          }
        )
    }

    success
  }
}