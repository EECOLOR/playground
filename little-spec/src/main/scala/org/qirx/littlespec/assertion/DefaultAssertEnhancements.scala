package org.qirx.littlespec.assertion

trait DefaultAssertEnhancements
  extends BasicAssertEnhancements
  with FragmentBodyAssertEnhancement
  with NumericAssertEnhancements { self:StaticAssertions => }