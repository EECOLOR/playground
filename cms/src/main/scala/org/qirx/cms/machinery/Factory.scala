package org.qirx.cms.machinery

import scala.language.higherKinds

/**
 * In the functional world called Pointed.
 * 
 * This `create` method of this pattern is also called (depending
 * on the context): `unit`, `point`, `pure` or `return`.
 */
trait Factory[T[_]] {
  def create[X](x:X):T[X]
  
  // useful Scala style alias
  def apply[X] = create[X] _
}