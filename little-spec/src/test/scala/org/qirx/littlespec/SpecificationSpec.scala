package org.qirx.littlespec

import org.qirx.littlespec.assertion.DefaultAssertEnhancements
import org.qirx.littlespec.assertion.DefaultAssertions
import org.qirx.littlespec.assertion.StaticAssertions

object SpecificationSpec extends Specification {

  "Specification should" - {

    "be of the correct type" - {
      val specification = new Specification {}
      specification must beAnInstanceOf[FragmentContainer with StaticAssertions with DefaultAssertEnhancements with DefaultAssertions]
    }
  }
}