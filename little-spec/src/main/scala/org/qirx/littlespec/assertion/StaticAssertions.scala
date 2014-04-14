package org.qirx.littlespec.assertion

import org.qirx.littlespec.Fragment

trait StaticAssertions {
  def success = Fragment.Body.Success
  val todo = Fragment.Body.Todo
  def pending(message:String) = Fragment.Body.Pending(message)
  def failure(message: String) = throw Fragment.ThrowableFailure(message)
}