package org.qirx.cms.construction

import org.qirx.cms.machinery.Program
import org.qirx.cms.machinery.~>
import org.qirx.cms.machinery.Id
import scala.language.higherKinds
import org.qirx.cms.machinery.Apply

class Branch[B] {
  case class Instance[A](value:Either[A, B])
}

object Branch {
  def apply[B] = new Branch[B]
}
//sealed trait Branching[A]

case class Branched[A, B](value: Either[A, B])// extends Branching[A]
