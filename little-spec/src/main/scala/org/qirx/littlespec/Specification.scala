package org.qirx.littlespec

import org.qirx.littlespec.assertion.DefaultAssertEnhancements
import org.qirx.littlespec.assertion.DefaultAssertions
import org.qirx.littlespec.assertion.StaticAssertions

abstract class Specification
  extends FragmentContainer
  with ExampleFragments
  with StaticAssertions
  with DefaultAssertEnhancements
  with DefaultAssertions {

  type FragmentBody = Fragment.Body
}