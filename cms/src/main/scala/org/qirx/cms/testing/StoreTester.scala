package org.qirx.cms.testing

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.higherKinds

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>

import play.api.libs.json.JsObject

class StoreTester[T[_]](
  implicit typeclass: TypeclassMagnet[T], ec: ExecutionContext) {

  type Result = (String, TestResult[T])

  def test(store: Store ~> Future)(
    implicit ev1: T[Option[JsObject]],
    ev2: T[Seq[JsObject]],
    ev3: T[Map[String, Seq[JsObject]]],
    ev4: T[Map[String, Option[JsObject]]],
    ev5: T[Boolean],
    ev6: T[Seq[Boolean]]): Seq[Result] = {

    val wrapper = new StoreWrapper {
      import Store._
      def list(metaId: String, fieldSet: Set[String] = Set.empty) = 
        store(List(metaId, fieldSet))
      def get(metaId: String, id: String, fieldSet: Set[String] = Set.empty) =
        store(Get(metaId, id, fieldSet))
      def exists(metaId: String, id: String) = 
        store(Exists(metaId, id))
      def save(metaId: String, id: String, document: JsObject)= 
        store(Save(metaId, id, document))
      def addId(metaId: String, id: String, newId: String) = 
        store(AddId(metaId, id, newId))
      def delete(metaId: String, id: String) = 
        store(Delete(metaId, id))
      def deleteAll(metaId: String) = 
        store(DeleteAll(metaId))
    }
    
    val tests = new StoreTests[T](wrapper)

    Await.result(tests.results, 2.second)
  }
}