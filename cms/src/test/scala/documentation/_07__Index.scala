package documentation

import org.qirx.littlespec.Specification
import org.qirx.cms.construction.Index
import org.qirx.cms.machinery.~>
import testUtils.codeString
import testUtils.cmsName
import play.api.libs.json.JsObject
import scala.concurrent.Future
import org.qirx.cms.testing.MemoryIndex
import org.qirx.cms.testing.IndexTester
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.qirx.cms.testing.TestResult
import org.qirx.cms.testing.TestFailure

class _07__Index extends Specification {

  "#The Index" - {

    val customIndexCode = codeString {
      object CustomIndex extends (Index ~> Future) {
        import Index._
        def transform[x] = {
          case List(metaId, fieldSet) => ???
          case Get(metaId, id, fieldSet) => ???
          case Put(metaId, id, document) => ???
          case AddId(metaId, id, newId) => ???
          case Delete(metaId, id) => ???
          case DeleteAll(metaId) => ???
          case Search(request, remainingPath) => ???
        }
      }
    }

    s"""|The index is used by the $cmsName to index documents. In practice it's
        |a transformation from `Index` elements to a Scala `Future`.
        |
        |More information about this type of transformation can be found at the 
        |`Store` section.
        |
        |To create an index simply implement the `transform` method.
        |```scala
        |$customIndexCode
        |```""".stripMargin - {

      val customIndex = new MemoryIndex

      """|To create an index implementation you can use the supplied `IndexTester`,
         |it will check if the index behaves as expected.
         |
         |Note that this will not check your implementation of `Search` as this 
         |is completely dependent on your index implementation.
         |
         |The index tester allows for typeclasses to be attached to the failures.
         |By default the `ClassTag` is attached.""".stripMargin -
        example {
          val indexTester = new IndexTester

          val testResults = indexTester.test(customIndex)

          testResults.foreach {
            case (description, result) =>
              result.fold(
                onSuccess = {
                  // report success using your favorite test framework
                },
                onFailure = {
                  case failure @ TestFailure(value, expectedValue) =>
                    // use the typeclass to get more information about the type
                    val className = failure.typeclass.runtimeClass.getName
                  // report failure using your favorite test framework
                }
              )
          }

          success
        }

      """|To use a different typeclass, simply pass it to the tester
         |
         |Note that when you do this, calling `test` requires you to 
         |supply the appropriate typeclass instances.
         |
         |The tests of the built-in stores make use of this feature""".stripMargin -
        example {
          trait CustomTypeclass[T]
          object CustomTypeclass {
            implicit val forBoolean: CustomTypeclass[Boolean] = null
            implicit val forJsObject: CustomTypeclass[JsObject] = null
            implicit val forJsObjectOption: CustomTypeclass[Option[JsObject]] = null
            implicit val forJsObjectSeq: CustomTypeclass[Seq[JsObject]] = null
            implicit def forJsObjectSeqMap: CustomTypeclass[Map[String, Seq[JsObject]]] = null
            implicit def forJsObjectOptionMap: CustomTypeclass[Map[String, Option[JsObject]]] = null
            implicit val forStringSeq: CustomTypeclass[Seq[String]] = null
            implicit val forBooleanSeq: CustomTypeclass[Seq[Boolean]] = null
          }

          val storeTester = new IndexTester[CustomTypeclass]

          val result: Seq[(String, TestResult[CustomTypeclass])] =
            storeTester.test(customIndex)

          success
        }
    }
  }

}