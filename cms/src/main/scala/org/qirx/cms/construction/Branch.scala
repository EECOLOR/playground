package org.qirx.cms.construction

/*
 * We can not use a type alias because that will confuse the 
 * type inference mechanism
 */
class Branch[B] {
  type T[x] = Instance[x]
  
  case class Instance[A](value:Either[A, B])
}

object Branch {
  def apply[B] = new Branch[B]
}
