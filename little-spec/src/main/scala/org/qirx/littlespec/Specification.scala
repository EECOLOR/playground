package org.qirx.littlespec

import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.reflect.ClassTag
import org.qirx.littlespec.assertion.StaticAssertions
import org.qirx.littlespec.assertion.BasicAssertEnhancements
import org.qirx.littlespec.assertion.NumericAssertEnhancements
import org.qirx.littlespec.assertion.DefaultAssertEnhancements
import org.qirx.littlespec.assertion.DefaultAssertions

trait Specification extends FragmentContainer
  with StaticAssertions
  with DefaultAssertEnhancements
  with DefaultAssertions