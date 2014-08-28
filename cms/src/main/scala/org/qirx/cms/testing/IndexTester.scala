package org.qirx.cms.testing

import org.qirx.cms.construction.Index
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import scala.language.higherKinds
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration._

class IndexTester[T[_]](
  implicit typeclass: TypeclassMagnet[T], ec: ExecutionContext) {

  import Index._

  type Result = (String, TestResult[T])

  def test(index: Index ~> Future)(
    implicit //ev1: T[JsObject],
    ev2: T[Option[JsObject]],
    ev3: T[Seq[JsObject]],
    ev4: T[Map[String, Seq[JsObject]]],
    ev5: T[Map[String, Option[JsObject]]],
    ev6: T[Seq[String]],
    ev7: T[Boolean],
    ev8: T[Seq[Boolean]]): Seq[Result] = {

    val t1 = "tests1"
    val t2 = "tests2"

    val tests: Seq[(String, TestCode[Future[TestResult[T]]])] =
      Seq(
        "be empty for the tests to run correctly" -> testCode {
          for {
            d1 <- index(List(t1))
            d2 <- index(List(t2))
          } yield {
            if (d1.isEmpty && d2.isEmpty) success
            else TestFailure(Map(
              t1 -> d1,
              t2 -> d2
            ), Map(
              t1 -> Seq.empty,
              t2 -> Seq.empty
            ))
          }
        },

        "accept an empty document" -> testCode {
          for {
            _ <- index(Put(t1, "empty", empty))
          } yield success
        },

        "accept a non-empty document" -> testCode {
          for {
            _ <- index(Put(t1, "non_empty", obj("some" -> "object")))
          } yield success
        },

        "simply override a complete document when an existing id is given" -> testCode {
          for {
            _ <- index(Put(t1, "non_empty", non_empty_1))
          } yield success
        },

        "be able to list those documents (order not relevant)" -> testCode {
          for {
            d <- index(List(t1))
          } yield {
            val e = Seq(empty, non_empty_1)
            if (e.diff(d).isEmpty) success
            else TestFailure(d, e)
          }
        },

        "keep documents with different metaId's separate" -> testCode {
          for {
            _ <- index(Put(t2, "non_empty", non_empty_2))
            d1 <- index(List(t1))
            d2 <- index(List(t2))
          } yield {
            val e1 = Seq(empty, non_empty_1)
            val e2 = Seq(non_empty_2)
            if (e1.diff(d1).isEmpty && e2.diff(d2).isEmpty) success
            else failure(
              Map(
                t1 -> d1,
                t2 -> d2
              ),
              Map(
                t1 -> e1,
                t2 -> e2
              ))
          }
        },

        "return `None` when trying to receive non-existent documents" -> testCode {
          for {
            d1 <- index(Get(t1, "non_existent"))
          } yield {
            val e1 = None
            if (d1 == e1) success
            else failure(d1, e1)
          }
        },

        "be able to retrieve single documents" -> testCode {
          for {
            d1 <- index(Get(t1, "empty"))
            d2 <- index(Get(t1, "non_empty"))
            d3 <- index(Get(t2, "non_empty"))
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
        },

        "be able to retrieve documents partially" -> testCode {
          for {
            d <- index(List(t1, Set("extra")))
          } yield {
            val e = Seq(empty, obj("extra" -> 1))
            if (e.diff(d).isEmpty) success
            else failure(d, e)
          }
        },

        "be able to retrieve a single document partially" -> testCode {
          for {
            d1 <- index(Get(t2, "non_empty", Set("extra")))
            d2 <- index(Get(t1, "empty", Set("extra")))
          } yield {
            val e1 = Some(obj("extra" -> 2))
            val e2 = Some(obj())
            if (e1 == d1 && e2 == d2) success
            else failure(Map(
              "non_empty" -> d1,
              "empty" -> d2
            ), Map(
              "non_empty" -> e1,
              "empty" -> e2
            ))
          }
        },

        "accept additional ids" -> testCode {
          for {
            _ <- index(AddId(t1, "non_empty", "non_empty_reference_1"))
            _ <- index(AddId(t1, "non_empty_reference_1", "non_empty_reference_2"))
          } yield success
        },

        "return the correct document for both the id and old ids" -> testCode {
          for {
            d1 <- index(Get(t1, "non_empty"))
            d2 <- index(Get(t1, "non_empty_reference_1"))
            d3 <- index(Get(t1, "non_empty_reference_2"))
          } yield {
            val e = Some(non_empty_1)
            if (e == d1 && e == d2 && e == d3) success
            else failure(
              Map(
                "non_empty" -> d1,
                "non_empty_reference_1" -> d2,
                "non_empty_reference_2" -> d3),
              Map(
                "non_empty" -> e,
                "non_empty_reference_1" -> e,
                "non_empty_reference_2" -> e
              )
            )
          }
        },

        "be able to save using an old id" -> testCode {
          for {
            _ <- index(Put(t1, "non_empty", non_empty_3))
            d <- index(Get(t1, "non_empty_reference_2"))
          } yield {
            val e = Some(non_empty_3)
            if (e == d) success
            else failure(d, e)
          }
        },

        "keep references between metaId's separate" -> testCode {
          for {
            _ <- index(AddId(t2, "non_empty", "non_empty_reference_3"))
            d1 <- index(Get(t1, "non_empty"))
            d2 <- index(Get(t2, "non_empty"))
          } yield {
            val e1 = Some(non_empty_3)
            val e2 = Some(non_empty_2)
            if (e1 == d1 && e2 == d2) success
            else failure(
              Map(
                t1 -> d1,
                t2 -> d2
              ),
              Map(
                t1 -> e1,
                t2 -> e2
              )
            )
          }
        },

        "be able to delete by id" -> testCode {
          for {
            _ <- index(Delete(t2, "non_empty_reference_3"))
            d1 <- index(List(t1))
            d2 <- index(List(t2))
          } yield {
            val e1 = Seq(empty, non_empty_3)
            val e2 = Seq.empty[JsObject]

            if (d1.diff(e1).isEmpty && d2.diff(e2).isEmpty) success
            else failure(
              Map(
                t1 -> d1,
                t2 -> d2),
              Map(
                t1 -> e1,
                t2 -> e2
              )
            )
          }
        },

        "be able to delete using an old id" -> testCode {
          for {
            _ <- index(Delete(t1, "non_empty"))
            d1 <- index(List(t1))
            d2 <- index(List(t2))
          } yield {
            val e1 = Seq(empty)
            val e2 = Seq.empty[JsObject]

            if (d1.diff(e1).isEmpty && d2.diff(e2).isEmpty) success
            else failure(
              Map(
                t1 -> d1,
                t2 -> d2),
              Map(
                t1 -> e1,
                t2 -> e2
              )
            )
          }
        },

        "have removed old ids" -> testCode {
          for {
            _ <- index(Put(t1, "non_empty", non_empty_1))
            d1 <- index(Get(t1, "non_empty_reference_1"))
            d2 <- index(Get(t1, "non_empty_reference_2"))
          } yield {
            val e = None
            if (d1 == e && d2 == e) success
            else failure(
              Map(
                "non_empty_reference_1" -> d1,
                "non_empty_reference_2" -> d1
              ),
              Map(
                "non_empty_reference_1" -> e,
                "non_empty_reference_2" -> e
              )
            )
          }
        },

        "be able to delete all documents" -> testCode {
          for {
            _ <- index(DeleteAll(t1))
            d1 <- index(List(t1))
            d2 <- index(List(t2))
          } yield {
            val e1 = Seq.empty[JsObject]
            val e2 = Seq.empty[JsObject]
            if (d1.diff(e1).isEmpty && d2.diff(e2).isEmpty) success
            else failure(
              Map(
                t1 -> d1,
                t2 -> d2),
              Map(
                t1 -> e1,
                t2 -> e2
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

    Await.result(testsInSequence, 2.second)
  }

  private class TestCode[T](code: => T) {
    def result = code
  }

  private def testCode[T](code: => T) = new TestCode(code)

  private val success: TestResult[T] = TestSuccess
  private def failure[A](value: A, expectedValue: A)(
    implicit e: T[A]) = TestFailure(value, expectedValue)

  private val empty = obj()
  private val non_empty_1 = obj("non" -> "empty 1", "extra" -> 1)
  private val non_empty_2 = obj("non" -> "empty 2", "extra" -> 2)
  private val non_empty_3 = obj("non" -> "empty 3", "extra" -> 3)

}