package org.qirx.cms.construction

import org.qirx.cms.machinery.Program
import org.qirx.cms.machinery.~>
import org.qirx.cms.machinery.Id
import scala.language.higherKinds
import org.qirx.cms.machinery.Apply

sealed trait Branching[T]

case class Branched[F[_], A, B](value: Program[F, Either[A, B]]) extends Branching[A]
