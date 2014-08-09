package org.qirx.cms

package object construction {
  type ValueOf[T] = Return[T]
  def ValueOf[T](value:T) = Return(value)
}