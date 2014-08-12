package org.qirx.cms.testing

import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.language.higherKinds
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._

class StoreTester[T[_]](
  implicit typeclass: TypeclassMagnet[T], ec: ExecutionContext) {

  import Store._

  type Result = (String, TestResult[T])

  def test(store: Store ~> Future)(
    implicit t1: T[JsObject],
    t2: T[Option[JsObject]],
    t3: T[Seq[JsObject]],
    t4: T[Map[String, Seq[JsObject]]],
    t5: T[Map[String, Option[JsObject]]]): Seq[Result] = {

    // d: document(s)
    // e: expected

    val tests: Seq[(String, TestCode[Future[TestResult[T]]])] =
      Seq(
        "be empty for the tests to run correctly" -> testCode {
          for {
            d <- store(List("tests1"))
          } yield {
            if (d.isEmpty) success
            else TestFailure(d, Seq.empty)
          }
        },

        "accept an empty document" -> testCode {
          for {
            _ <- store(Save("tests1", "empty", empty))
          } yield success
        },

        "accept a non-empty document" -> testCode {
          for {
            _ <- store(Save("tests1", "non_empty", non_empty_1))
          } yield success
        },

        "be able to list those documents in the order they were inserted" -> testCode {
          for {
            d <- store(List("tests1"))
          } yield {
            val e = Seq(empty, non_empty_1)
            if (d == e) success
            else TestFailure(d, e)
          }
        },

        "keep documents with different metaId's separate" -> testCode {
          for {
            _ <- store(Save("tests2", "non_empty", non_empty_2))
            d1 <- store(List("tests1"))
            d2 <- store(List("tests2"))
          } yield {
            val e1 = Seq(empty, non_empty_1)
            val e2 = Seq(non_empty_2)
            if (d1 == e1 && d2 == e2) success
            else failure(
              Map(
                "tests1" -> d1,
                "tests2" -> d2
              ),
              Map(
                "tests1" -> e1,
                "tests2" -> e2
              ))
          }
        },

        "return `None` when trying to receive non-existent documents" -> testCode {
          for {
            d1 <- store(Get("tests1", "non_existent"))
          } yield {
            val e1 = None
            if (d1 == e1) success
            else failure(d1, e1)
          }
        },

        "be able to recieve single documents" -> testCode {
          for {
            d1 <- store(Get("tests1", "empty"))
            d2 <- store(Get("tests1", "non_empty"))
            d3 <- store(Get("tests2", "non_empty"))
          } yield {
            val e1 = Some(empty)
            val e2 = Some(non_empty_1)
            val e3 = Some(non_empty_2)
            if (d1 == e1 && d2 == e2 && d3 == e3) success
            else failure(
              Map(
                "tests1 - empty" -> d1,
                "tests1 - non_empty" -> d2,
                "tests2 - non_empty" -> d3
              ),
              Map(
                "tests1 - empty" -> e1,
                "tests1 - non_empty" -> e2,
                "tests2 - non_empty" -> e3
              )
            )
          }
        }
      )

    val testsInSequence =
      tests.foldLeft(Future successful Seq.empty[Result]) {
        case (previousResultFuture, (description, testCode)) =>
          for {
            previousResults <- previousResultFuture
            testResult <- testCode.result
          } yield {
            previousResults :+ (description -> testResult)
          }
      }

    Await.result(testsInSequence, 1.second)
  }

  private class TestCode[T](code: => T) {
    def result = code
  }

  private def testCode[T](code: => T) = new TestCode(code)

  private val success: TestResult[T] = TestSuccess
  private def failure[A](value: A, expectedValue: A)(
    implicit e: T[A]) = TestFailure(value, expectedValue)

  private val empty = obj()
  private val non_empty_1 = obj("non" -> "empty 1")
  private val non_empty_2 = obj("non" -> "empty 1")

}