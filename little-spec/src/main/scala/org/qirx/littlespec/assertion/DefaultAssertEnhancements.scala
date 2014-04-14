package org.qirx.littlespec.assertion

trait DefaultAssertEnhancements
  extends BasicAssertEnhancements
  with NumericAssertEnhancements { self:StaticAssertions => }