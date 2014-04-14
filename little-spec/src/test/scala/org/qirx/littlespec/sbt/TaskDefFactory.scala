package org.qirx.littlespec.sbt

import sbt.testing.{ TaskDef => SbtTaskDef }
import sbt.testing.Fingerprint
import sbt.testing.SubclassFingerprint
import sbt.testing.TestSelector

object TaskDefFactory {
  def create(qualifiedName: String, isObject: Boolean = false) = {
    val fingerprint =
      new SubclassFingerprint {
        val isModule = isObject
        val requireNoArgConstructor = true
        val superclassName = "org.qirx.littlespec.Specification"
      }
    new SbtTaskDef(qualifiedName, fingerprint, false, Array(new TestSelector(qualifiedName)))
  }
}