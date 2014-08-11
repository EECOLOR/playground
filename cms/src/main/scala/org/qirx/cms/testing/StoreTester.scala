package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.language.higherKinds
import play.api.libs.json.JsObject

class StoreTester[T[_]](
  implicit typeclass: TypeclassMagnet[T]) {

  def test(store: Store ~> Future)(
    implicit e1: T[JsObject]): Seq[(String, TestResult[Any, T])] = Seq.empty
}