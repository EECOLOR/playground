package org.qirx.cms.testing

import scala.language.higherKinds
import org.qirx.cms.machinery.Id
import scala.reflect.ClassTag

trait TypeclassMagnet[T[_]] {
  type Type[x] = T[x]
}

trait AnyMagnet {
  implicit def any[T[_]]:TypeclassMagnet[T] = null
}

object TypeclassMagnet extends AnyMagnet {
  implicit val classTag: TypeclassMagnet[ClassTag] = null
}