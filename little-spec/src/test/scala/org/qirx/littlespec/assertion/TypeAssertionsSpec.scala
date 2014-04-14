package org.qirx.littlespec.assertion

import org.qirx.littlespec.Specification
import testUtils.beAFailure

object TypeAssertionsSpec extends Specification {

  "TypeAssertions should" - {

    "provide a 'be an instance of' assertion" - {

      "that fails if the object is not an instance of the given type" - {
        beAnInstanceOf[String].assert(1) is
          Left("Integer is not an instance of String")

        beAnInstanceOf[CustomType1].assert(new CustomInstance1) is
          Left("CustomInstance1 is not an instance of CustomType1")
      }

      "that succeeds if the object is an instance of the given type" - {
        beAnInstanceOf[String].assert("") is
          Right(success)

        beAnInstanceOf[CustomType2].assert(new CustomInstance2) is
          Right(success)
      }
    }
  }
}

trait CustomType1
class CustomInstance1
trait CustomType2
class CustomInstance2 extends CustomType2