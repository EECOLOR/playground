package org.qirx.cms.testing

import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.concurrent.Future

class StoreTests[T[_]](storeWrapper: StoreWrapper)(
  implicit ec: ExecutionContext,
  ev1: T[Map[String, Seq[JsObject]]],
  ev2: T[Seq[JsObject]],
  ev3: T[Option[JsObject]],
  ev4: T[Map[String, Option[JsObject]]],
  ev5: T[Boolean],
  ev6: T[Seq[Boolean]]) {

  import storeWrapper._

  val t1 = "tests1"
  val t2 = "tests2"

  private val empty = obj()
  private val non_empty = obj("non" -> "empty")
  private val non_empty_1 = obj("non" -> "empty 1", "extra" -> 1)
  private val non_empty_2 = obj("non" -> "empty 2", "extra" -> 2)
  private val non_empty_3 = obj("non" -> "empty 3", "extra" -> 3)

  private val allTests =
    Seq(
      startEmpty,
      acceptEmptyDocument,
      acceptNonEmptyDocument,
      overrideDocument,
      listDocuments,
      keepMetaIdSeparate,
      getNonExistentDocument,
      singleDocument,
      partialList,
      partialGet,
      addAdditionalId,
      getByOldId,
      saveWithOldId,
      keepMetaIdReferenceSeparate,
      existsNotFound,
      existsFound,
      deleteById,
      deleteByOldId,
      referenceRemovedOnDelete,
      deleteAllDocuments,
      referenceRemovedOnDeleteAll)

  def results = {
    val testsInSequence =
      allTests.foldLeft(Future successful Seq.empty[(String, TestResult[T])]) {
        case (previousResultFuture, (description, testCode)) =>
          for {
            previousResults <- previousResultFuture
            testResult <- testCode.result
          } yield {
            previousResults :+ (description -> testResult)
          }
      }

    testsInSequence
  }

  private def startEmpty =
    "be empty for the tests to run correctly" -> testCode {
      for {
        d1 <- list(t1)
        d2 <- list(t2)
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
    }

  private def acceptEmptyDocument =
    "accept an empty document" -> testCode {
      for {
        _ <- save(t1, "empty", empty)
      } yield success
    }

  private def acceptNonEmptyDocument =
    "accept a non-empty document" -> testCode {
      for {
        _ <- save(t1, "non_empty", non_empty)
      } yield success
    }

  private def overrideDocument =
    "simply override a complete document when an existing id is given" -> testCode {
      for {
        _ <- save(t1, "non_empty", non_empty_1)
      } yield success
    }

  private def listDocuments =
    "be able to list those documents (order not relevant)" -> testCode {
      for {
        d <- list(t1)
      } yield {
        val e = Seq(empty, non_empty_1)
        if (e.diff(d).isEmpty) success
        else TestFailure(d, e)
      }
    }

  private def keepMetaIdSeparate =
    "keep documents with different metaId's separate" -> testCode {
      for {
        _ <- save(t2, "non_empty", non_empty_2)
        d1 <- list(t1)
        d2 <- list(t2)
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
    }

  private def getNonExistentDocument =
    "return `None` when trying to receive non-existent documents" -> testCode {
      for {
        d1 <- get(t1, "non_existent")
      } yield {
        val e1 = None
        if (d1 == e1) success
        else failure(d1, e1)
      }
    }

  private def singleDocument =
    "be able to retrieve single documents" -> testCode {
      for {
        d1 <- get(t1, "empty")
        d2 <- get(t1, "non_empty")
        d3 <- get(t2, "non_empty")
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

  private def partialList =
    "be able to retrieve documents partially" -> testCode {
      for {
        d <- list(t1, Set("extra"))
      } yield {
        val e = Seq(empty, obj("extra" -> 1))
        if (e.diff(d).isEmpty) success
        else failure(d, e)
      }
    }

  private def partialGet =
    "be able to retrieve a single document partially" -> testCode {
      for {
        d1 <- get(t2, "non_empty", Set("extra"))
        d2 <- get(t1, "empty", Set("extra"))
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
    }

  private def addAdditionalId =
    "accept additional ids" -> testCode {
      for {
        _ <- addId(t1, "non_empty", "non_empty_reference_1")
        _ <- addId(t1, "non_empty_reference_1", "non_empty_reference_2")
      } yield success
    }

  private def getByOldId =
    "return the correct document for both the id and old ids" -> testCode {
      for {
        d1 <- get(t1, "non_empty")
        d2 <- get(t1, "non_empty_reference_1")
        d3 <- get(t1, "non_empty_reference_2")
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
    }

  private def saveWithOldId =
    "be able to save using an old id" -> testCode {
      for {
        _ <- save(t1, "non_empty", non_empty_3)
        d <- get(t1, "non_empty_reference_2")
      } yield {
        val e = Some(non_empty_3)
        if (e == d) success
        else failure(d, e)
      }
    }

  private def keepMetaIdReferenceSeparate =
    "keep references between metaId's separate" -> testCode {
      for {
        _ <- addId(t2, "non_empty", "non_empty_reference_3")
        d1 <- get(t1, "non_empty")
        d2 <- get(t2, "non_empty")
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
    }

  private def existsNotFound =
    "return `false` when checking the existence of a non-existent document" -> testCode {
      for {
        b <- exists(t1, "non_existent")
      } yield {
        if (b == false) success
        else failure(b, false)
      }
    }

  private def existsFound =
    "return `true` on existing documents" -> testCode {
      for {
        b1 <- exists(t1, "non_empty")
        b2 <- exists(t1, "non_empty_reference_1")
        b3 <- exists(t1, "non_empty_reference_2")
        b4 <- exists(t2, "non_empty")
        b5 <- exists(t2, "non_empty_reference_3")
      } yield {
        val e = true
        if (b1 == e && b2 == e && b3 == e && b4 == e && b5 == e) success
        else failure(Seq(b1, b2, b3, b4, b5), Seq.fill(5)(true))
      }
    }

  private def deleteById =
    "be able to delete by id" -> testCode {
      for {
        _ <- delete(t2, "non_empty_reference_3")
        d1 <- list(t1)
        d2 <- list(t2)
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
    }

  private def deleteByOldId =
    "be able to delete using an old id" -> testCode {
      for {
        _ <- delete(t1, "non_empty")
        d1 <- list(t1)
        d2 <- list(t2)
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
    }

  private def referenceRemovedOnDelete =
    "have removed old ids" -> testCode {
      for {
        _ <- save(t1, "non_empty", non_empty_1)
        b1 <- exists(t1, "non_empty_reference_1")
        b2 <- exists(t1, "non_empty_reference_2")
      } yield {
        val e = false
        if (b1 == e && b2 == e) success
        else failure(
          Seq(b1, b2),
          Seq(false, false)
        )
      }
    }

  private def deleteAllDocuments =
    "be able to delete all documents" -> testCode {
      for {
        _ <- save(t2, "non_empty", non_empty_2)
        _ <- addId(t2, "non_empty", "non_empty_reference_3")
        _ <- deleteAll(t1)
        _ <- deleteAll(t2)
        d1 <- list(t1)
        d2 <- list(t2)
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

  private def referenceRemovedOnDeleteAll =
    "have removed old ids" -> testCode {
      for {
        _ <- save(t2, "non_empty", non_empty_2)
        b <- exists(t2, "non_empty_reference_3")
        _ <- deleteAll(t2)
      } yield {
        val e = false
        if (b == e) success
        else failure(b, e)
      }
    }

  private class TestCode[T](code: => T) {
    def result = code
  }

  private def testCode[T](code: => T) = new TestCode(code)

  private val success: TestResult[T] = TestSuccess
  private def failure[A](value: A, expectedValue: A)(
    implicit e: T[A]) = TestFailure(value, expectedValue)
}