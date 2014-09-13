package documentation

import org.qirx.littlespec.Specification
import testUtils.cmsName
import testUtils.codeString
import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>
import scala.concurrent.Future
import org.qirx.cms.testing.MemoryStore
import org.qirx.cms.testing.StoreTester
import org.qirx.cms.testing.TestSuccess
import org.qirx.cms.testing.TestFailure
import scala.reflect.ClassTag
import play.api.libs.json.JsObject
import org.qirx.cms.testing.TestResult
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import org.qirx.cms.testing.TypeclassMagnet
import org.qirx.cms.elasticsearch

class _06__Store extends Specification {

  "#The Store" - {

    val customStoreCode = codeString {
      object CustomStore extends (Store ~> Future) {
        import Store._
        def transform[x] = {
          case List(metaId, fieldSet) => ???
          case Get(metaId, id, fieldSet) => ???
          case Save(metaId, id, document) => ???
          case AddId(metaId, id, newId) => ???
          case Delete(metaId, id) => ???
          case DeleteAll(metaId) => ???
          case Exists(metaId, id) => ???
        }
      }
    }

    s"""|The store is used by the $cmsName to store documents. In practice it's
        |a transformation from `Store` elements to a Scala `Future`.
        |
        |Since both `Store` and `Future` have a single type parameter, we call 
        |this transformation a natural transformation. Where a function has a
        |type signature `A => B`, natural transformation has a type signature 
        |`A[x] => B[x]`.
        |
        |We have introduced an operator for natural transformations, this allows
        |us to write the store signature like this: `Store ~> Future`.
        |
        |`Store[T]` is a sealed trait where `T` is the type of the expected result. 
        |The `Get` case extends `Store[Option[JsObject]]` meaning it should 
        |return an `Option` of `JsObject`.
        |
        |To create a store simply implement the `transform` method.
        |```scala
        |$customStoreCode
        |```""".stripMargin - {

      s"An in memory version is provide as `${classOf[MemoryStore].getName}`" - {
        implicitly[MemoryStore <:< (Store ~> Future)]
        success
      }

      s"An Elastic Search version is provide as `${classOf[elasticsearch.Store].getName}`" - {
        implicitly[elasticsearch.Store <:< (Store ~> Future)]
        success
      }

      val customStore = new MemoryStore

      """|To create a custom store implementation you can use the supplied `StoreTester`,
         |it will check if the store behaves as expected.
         |
         |Some test frameworks work better when they have extra information about 
         |the type. Most frameworks make use of typeclasses to help with this. 
         |
         |The store tester allows for typeclasses to be attached to the failures.
         |By default the `TypeclassMagnet.None` is attached, which does not 
         |contain any information.""".stripMargin -
        example {
          val storeTester = new StoreTester

          val testResults = storeTester.test(customStore)

          testResults.foreach {
            case (description, result) =>
              result.fold(
                onSuccess = {
                  // report success using your favorite test framework
                },
                onFailure = {
                  case failure @ TestFailure(value, expectedValue) =>
                    // use the typeclass to get more information about the type
                    val none: TypeclassMagnet.None[_] = failure.typeclass
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
            implicit val forJsObjectOption: CustomTypeclass[Option[JsObject]] = null
            implicit val forJsObjectSeq: CustomTypeclass[Seq[JsObject]] = null
            implicit def forJsObjectSeqMap: CustomTypeclass[Map[String, Seq[JsObject]]] = null
            implicit def forJsObjectOptionMap: CustomTypeclass[Map[String, Option[JsObject]]] = null
            implicit val forBooleanSeq: CustomTypeclass[Seq[Boolean]] = null
          }

          val storeTester = new StoreTester[CustomTypeclass]

          val result: Seq[(String, TestResult[CustomTypeclass])] =
            storeTester.test(customStore)

          success
        }
    }
  }
}