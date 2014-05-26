package org.qirx.littlespec

import scala.concurrent.duration.FiniteDuration

sealed trait Result

case class Pending(title:String, message:String) extends Result

case class Success(title:String)(val duration:FiniteDuration) extends Result

case class UnexpectedFailure(title:String, t:Throwable) extends Result

case class Failure(title:String, message:String, f:Fragment.ThrowableFailure) extends Result

case class CompoundResult(title:String, results:Seq[Result]) extends Result
