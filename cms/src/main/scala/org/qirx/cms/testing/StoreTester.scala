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
    implicit ev1: T[JsObject],
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
            d1 <- store(List(t1))
            d2 <- store(List(t2))
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
            _ <- store(Save(t1, "empty", empty))
          } yield success
        },

        "accept a non-empty document" -> testCode {
          for {
            _ <- store(Save(t1, "non_empty", obj("some" -> "object")))
          } yield success
        },

        "simply override a complete document when an existing id is given" -> testCode {
          for {
            _ <- store(Save(t1, "non_empty", non_empty_1))
          } yield success
        },

        "be able to list those documents (order not relevant)" -> testCode {
          for {
            d <- store(List(t1))
          } yield {
            val e = Seq(empty, non_empty_1)
            if (e.diff(d).isEmpty) success
            else TestFailure(d, e)
          }
        },

        "keep documents with different metaId's separate" -> testCode {
          for {
            _ <- store(Save(t2, "non_empty", non_empty_2))
            d1 <- store(List(t1))
            d2 <- store(List(t2))
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
            d1 <- store(Get(t1, "non_existent"))
          } yield {
            val e1 = None
            if (d1 == e1) success
            else failure(d1, e1)
          }
        },

        "be able to retrieve single documents" -> testCode {
          for {
            d1 <- store(Get(t1, "empty"))
            d2 <- store(Get(t1, "non_empty"))
            d3 <- store(Get(t2, "non_empty"))
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
            d <- store(List(t1, Set("extra")))
          } yield {
            val e = Seq(empty, obj("extra" -> 1))
            if (e.diff(d).isEmpty) success
            else failure(d, e)
          }
        },

        "be able to retrieve a single document partially" -> testCode {
          for {
            d1 <- store(Get(t2, "non_empty", Set("extra")))
            d2 <- store(Get(t1, "empty", Set("extra")))
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
            _ <- store(AddId(t1, "non_empty", "non_empty_reference_1"))
            _ <- store(AddId(t1, "non_empty_reference_1", "non_empty_reference_2"))
          } yield success
        },

        "return the correct document for both the id and old ids" -> testCode {
          for {
            d1 <- store(Get(t1, "non_empty"))
            d2 <- store(Get(t1, "non_empty_reference_1"))
            d3 <- store(Get(t1, "non_empty_reference_2"))
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
            _ <- store(Save(t1, "non_empty", non_empty_3))
            d <- store(Get(t1, "non_empty_reference_2"))
          } yield {
            val e = Some(non_empty_3)
            if (e == d) success
            else failure(d, e)
          }
        },

        "keep references between metaId's separate" -> testCode {
          for {
            _ <- store(AddId(t2, "non_empty", "non_empty_reference_3"))
            d1 <- store(Get(t1, "non_empty"))
            d2 <- store(Get(t2, "non_empty"))
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

        "return `false` when checking the existence of a non-existent document" -> testCode {
          for {
            b <- store(Exists(t1, "non_existent"))
          } yield {
            if (b == false) success
            else failure(b, false)
          }
        },

        "return `true` on existing documents" -> testCode {
          for {
            b1 <- store(Exists(t1, "non_empty"))
            b2 <- store(Exists(t1, "non_empty_reference_1"))
            b3 <- store(Exists(t1, "non_empty_reference_2"))
            b4 <- store(Exists(t2, "non_empty"))
            b5 <- store(Exists(t2, "non_empty_reference_3"))
          } yield {
            val e = true
            if (b1 == e && b2 == e && b3 == e && b4 == e && b5 == e) success
            else failure(Seq(b1, b2, b3, b4, b5), Seq.fill(5)(true))
          }
        },

        "be able to delete by id" -> testCode {
          for {
            _ <- store(Delete(t2, "non_empty_reference_3"))
            d1 <- store(List(t1))
            d2 <- store(List(t2))
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
            _ <- store(Delete(t1, "non_empty"))
            d1 <- store(List(t1))
            d2 <- store(List(t2))
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
            _ <- store(Save(t1, "non_empty", non_empty_1))
            b1 <- store(Exists(t1, "non_empty_reference_1"))
            b2 <- store(Exists(t1, "non_empty_reference_2"))
          } yield {
            val e = false
            if (b1 == e && b2 == e) success
            else failure(
              Seq(b1, b2),
              Seq(false, false)
            )
          }
        },

        "be able to delete all documents" -> testCode {
          for {
            _ <- store(DeleteAll(t1))
            d1 <- store(List(t1))
            d2 <- store(List(t2))
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