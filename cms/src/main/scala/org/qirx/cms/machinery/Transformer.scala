package org.qirx.cms.machinery

import scala.language.higherKinds

/**
 * In the functional world called Functor.
 * 
 * The method `transform` is also called `fmap` or `map`
 */
trait Transformer[T[_]] extends Factory[T] {
  def transform[A, B](t:T[A], using:A => B):T[B]
}