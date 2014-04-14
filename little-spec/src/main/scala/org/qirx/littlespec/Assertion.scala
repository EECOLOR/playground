package org.qirx.littlespec

trait Assertion[T] {
  def assert(s: => T): Either[String, Fragment.Body]
}