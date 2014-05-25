package org.qirx.littlespec.sbt

import sbt.testing.{ TaskDef => SbtTaskDef }
import sbt.testing.Fingerprint
import sbt.testing.SubclassFingerprint
import sbt.testing.TestSelector

object TaskDefFactory {

  case class FingerPrint(isObject: Boolean) extends SubclassFingerprint {
    val isModule = isObject
    val requireNoArgConstructor = true
    val superclassName = "org.qirx.littlespec.Specification"
  }

  def create(qualifiedName: String, isObject: Boolean = false) =
    new SbtTaskDef(
        qualifiedName,
        FingerPrint(isObject),
        false,
        Array(new TestSelector(qualifiedName)))
}