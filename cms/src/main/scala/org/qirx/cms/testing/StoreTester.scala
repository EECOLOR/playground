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
    t5: T[Map[String, Option[JsObject]]],
    t6: T[Seq[String]],
    t7: T[Boolean],
    t8: T[Seq[Boolean]]
    ): Seq[Result] = {

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
            _ <- store(Save("tests1", "non_empty", obj("some" -> "object")))
          } yield success
        },

        "simply override a complete document when an existing id is given" -> testCode {
          for {
            _ <- store(Save("tests1", "non_empty", non_empty_1))
          } yield success
        },

        "be able to list those documents (order not relevant)" -> testCode {
          for {
            d <- store(List("tests1"))
          } yield {
            val e = Seq(empty, non_empty_1)
            if (e.diff(d).isEmpty) success
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
            if (e1.diff(d1).isEmpty && e2.diff(d2).isEmpty) success
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

        "be able to retrieve single documents" -> testCode {
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
        },

        "be able to retrieve documents partially" -> testCode {
          for {
            d <- store(List("tests1", Set("extra")))
          } yield {
            val e = Seq(empty, obj("extra" -> 1))
            if (e.diff(d).isEmpty) success
            else failure(d, e)
          }
        },

        "be able to retrieve a single document partially" -> testCode {
          for {
            d <- store(Get("tests2", "non_empty", Set("extra")))
          } yield {
            val e = Some(obj("extra" -> 2))
            if (e == d) success
            else failure(d, e)
          }
        },

        "accept updates of the id" -> testCode {
          for {
            _ <- store(UpdateId("tests1", "non_empty", "non_empty_reference_1"))
            _ <- store(UpdateId("tests1", "non_empty_reference_1", "non_empty_reference_2"))
          } yield success
        },

        "return the correct document for both the id and old ids" -> testCode {
          for {
            d1 <- store(Get("tests1", "non_empty"))
            d2 <- store(Get("tests1", "non_empty_reference_1"))
            d3 <- store(Get("tests1", "non_empty_reference_2"))
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

        "keep references between metaId's separate" -> testCode {
          for {
            _ <- store(UpdateId("tests2", "non_empty", "non_empty_reference_3"))
            d1 <- store(Get("tests1", "non_empty"))
            d2 <- store(Get("tests2", "non_empty"))
          } yield {
            val e1 = Some(non_empty_1)
            val e2 = Some(non_empty_2)
            if (e1 == d1 && e2 == d2) success
            else failure(
              Map(
                "tests1" -> d1,
                "tests2" -> d2
              ),
              Map(
                "tests1" -> e1,
                "tests2" -> e2
              )
            )
          }
        },

        "keep track of the actual id's" -> testCode {
          for {
            id1 <- store(GetActualId("tests1", "non_empty"))
            id2 <- store(GetActualId("tests1", "non_empty_reference_1"))
            id3 <- store(GetActualId("tests1", "non_empty_reference_2"))
            id4 <- store(GetActualId("tests2", "non_empty"))
            id5 <- store(GetActualId("tests2", "non_empty_reference_3"))
          } yield {
            val e1 = Some("non_empty_reference_2")
            val e2 = Some("non_empty_reference_3")
            if (id1 == e1 && id2 == e1 && id3 == e1 && id4 == e2 && id5 == e2)
              success
            else failure(
              Seq(id1, id2, id3, id4, id5).flatten,
              Seq(e1, e1, e1, e2, e2).flatten
            )
          }
        },

        "return `false` when checking the existence of a non-existent document" -> testCode {
         for {
           b <- store(Exists("tests1", "non_existent"))
         } yield {
           if (b == false) success
           else failure(b, false)
         }
        },
        
        "return `true` on existing documents" -> testCode {
          for {
           b1 <- store(Exists("tests1", "non_empty"))
            b2 <- store(Exists("tests1", "non_empty_reference_1"))
            b3 <- store(Exists("tests1", "non_empty_reference_2"))
            b4 <- store(Exists("tests2", "non_empty"))
            b5 <- store(Exists("tests2", "non_empty_reference_3"))
          } yield {
            val e = true
            if (b1 == e && b2 == e && b3 == e && b4 == e && b5 == e) success
            else failure(Seq(b1, b2, b3, b4, b5), Seq.fill(5)(true))
          }
        },
        
        "be able to delete by id" -> testCode {
          for {
            _ <- store(Delete("tests2", Some("non_empty")))
            d1 <- store(List("tests1"))
            d2 <- store(List("tests2"))
          } yield {
            val e1 = Seq(empty, non_empty_1)
            val e2 = Seq.empty[JsObject]

            if (d1.diff(e1).isEmpty && d2.diff(e2).isEmpty) success
            else failure(
              Map(
                "tests1" -> d1,
                "tests2" -> d2),
              Map(
                "tests1" -> e1,
                "tests2" -> e2
              )
            )
          }
        },

        "be able to delete all documents" -> testCode {
          for {
            _ <- store(Delete("tests1"))
            d1 <- store(List("tests1"))
            d2 <- store(List("tests2"))
          } yield {
            val e1 = Seq.empty[JsObject]
            val e2 = Seq.empty[JsObject]
            if (d1.diff(e1).isEmpty && d2.diff(e2).isEmpty) success
            else failure(
              Map(
                "tests1" -> d1,
                "tests2" -> d2),
              Map(
                "tests1" -> e1,
                "tests2" -> e2
              )
            )
          }
        },

        "retrieving the actual id's should return None" -> testCode {
          for {
            id1 <- store(GetActualId("tests1", "non_empty"))
            id2 <- store(GetActualId("tests1", "non_empty_reference_1"))
            id3 <- store(GetActualId("tests1", "non_empty_reference_2"))
            id4 <- store(GetActualId("tests2", "non_empty"))
            id5 <- store(GetActualId("tests2", "non_empty_reference_3"))
          } yield {
            val e = None
            if (id1 == e && id2 == e && id3 == e && id4 == e && id5 == e) success
            else failure(
              Seq(id1, id2, id3, id4, id5).flatten,
              Seq.empty
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
  private val non_empty_1 = obj("non" -> "empty 1", "extra" -> 1)
  private val non_empty_2 = obj("non" -> "empty 2", "extra" -> 2)

}