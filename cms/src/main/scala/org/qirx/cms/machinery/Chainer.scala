package org.qirx.cms.machinery

import scala.language.higherKinds

/**
 * In the functional world called Bind. If you combine this with 
 * Factory and Transformer it's called Monad.
 * 
 * The method `chain` is also known as `bind`, `flatMap` and `>>=`
 */
trait Chainer[T[_]] {

  def chain[A, B](t: T[A], to: A => T[B]): T[B]
}
