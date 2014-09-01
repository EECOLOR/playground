package org.qirx.cms.testing

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.language.higherKinds

import org.qirx.cms.construction.Index
import org.qirx.cms.machinery.~>

import play.api.libs.json.JsObject

class IndexTester[T[_]](
  implicit typeclass: TypeclassMagnet[T], ec: ExecutionContext) {

  type Result = (String, TestResult[T])

  def test(index: Index ~> Future)(
    implicit ev1: T[Option[JsObject]],
    ev2: T[Seq[JsObject]],
    ev3: T[Map[String, Seq[JsObject]]],
    ev4: T[Map[String, Option[JsObject]]],
    ev5: T[Boolean],
    ev6: T[Seq[Boolean]]): Seq[Result] = {

    val wrapper = new StoreWrapper {
      import Index._
      def list(metaId: String, fieldSet: Set[String] = Set.empty) = 
        index(List(metaId, fieldSet))
      def get(metaId: String, id: String, fieldSet: Set[String] = Set.empty) =
        index(Get(metaId, id, fieldSet))
      def exists(metaId: String, id: String) = 
        index(Exists(metaId, id))
      def save(metaId: String, id: String, document: JsObject)= 
        index(Put(metaId, id, document))
      def addId(metaId: String, id: String, newId: String) = 
        index(AddId(metaId, id, newId))
      def delete(metaId: String, id: String) = 
        index(Delete(metaId, id))
      def deleteAll(metaId: String) = 
        index(DeleteAll(metaId))
    }
    
    val tests = new StoreTests[T](wrapper)

    Await.result(tests.results, 2.second)
  }
}