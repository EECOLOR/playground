package org.qirx.cms.testing

import scala.language.higherKinds
import org.qirx.cms.machinery.Id
import scala.reflect.ClassTag

trait TypeclassMagnet[T[_]]

trait AnyMagnet {
  implicit def any[T[_]]:TypeclassMagnet[T] = null
}

object TypeclassMagnet extends AnyMagnet {
  trait None[T]
  object None {
    implicit def any[T]:None[T] = null
  }
  
  implicit val default: TypeclassMagnet[None] = null
}