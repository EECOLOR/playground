package documentation

import scala.concurrent.Future
import org.qirx.cms.Cms
import org.qirx.littlespec.Specification
import play.api.mvc.RequestHeader
import testUtils.Example
import testUtils.TestStore
import org.qirx.cms.metadata.properties.Label
import org.qirx.cms.metadata.dsl.Document
import testUtils.TestEnvironment
import org.qirx.cms.construction.Store.Save
import play.api.libs.json.Json.obj
import play.api.libs.json.Json.arr
import play.api.libs.json.__
import scala.concurrent.Await
import scala.concurrent.duration._
import org.qirx.cms.Environment
import scala.collection.mutable.ListBuffer
import play.api.libs.json.JsObject
import org.qirx.cms.metadata.DocumentMetadata
import play.api.test.Helpers
import testUtils.TestApplication
import play.api.libs.json.JsValue
import testUtils.PostToApplication
import testUtils.TestIndex

object _05_Evolution extends Specification with Example {

  """|#Evolution
     |
     |An important part of an application is it's ability to evolve. This means 
     |that your domain model will change. Evolution of the model is a tricky 
     |subject because documents have been stored with a previous version of the 
     |model.
     |
     |There are different ways to solve the problems associated with this fact.
     |I chose for a very simple way to solve the problem. Note that this is not 
     |the best performing solution, but it is the easiest implementation.
     |
     |The metadata is available as Scala code. This means we have no way of 
     |detecting if it has changed. So in order to detect change we check 
     |the metadata against all documents as soon as an instance of the 
     |`$cmsName` is being created.""".stripMargin - {

    val pathPrefix = "/api"
    val authenticate: RequestHeader => Future[Boolean] = { _ => Future.successful(true) }

    val testStore = new TestStore
    val testIndex = new TestIndex
    val testEnvironment =
      new Environment {
        val store = testStore
        val index = testIndex
        def reportDocumentMetadataMismatch(document: JsObject, meta: DocumentMetadata, validationResults: Seq[JsObject]): Unit =
          ???
      }

    val documentMetadata = codeString {
      Document(id = "article", idField = "header")(
        "header" -> Label
      )
    }
    val cms =
      new Cms(pathPrefix, authenticate,
        environment = testEnvironment,
        documents = Seq(documentMetadata.value)
      )
    val testApplication = TestApplication(cms)

    val POST = new PostToApplication(testApplication, pathPrefix + "/private")

    s"""|Before we can can continue, let's first insert a document into the store
        |
        |Note that the metadata currently is:
        |```scala
        |$documentMetadata
        |```""".stripMargin - example {
      val document = obj("header" -> "Article 1")
      val (status, body) = POST(document) to "/article"

      status is 201
      body is obj(
        "id" -> "article_1"
      )
    }

    """|The first problem arises when the new metadata makes previously saved 
       |documents invalid. Since we are checking stuff runtime we need some way 
       |to report this problem. Of course we need to try and prevent that from 
       |happening, but since we are human, we need to get notified in case we
       |make a mistake. More on preventing mistakes in later in this document.
       |
       |Note that a method in the `Environment` is used to report the error.
       |
       |As you might have expected, code like the following can be used to test 
       |if your evolution works with the expected store. In your real test you 
       |will of course point your store to the production data. How you do that 
       |is up to you, I can imagine you simply start a `FakeApplication` with 
       |your actual `GlobalSettings` instance in the context of the production
       |configuration.
       |""".stripMargin -
      example {
        val reports = ListBuffer.empty[(JsObject, DocumentMetadata, Seq[JsObject])]
        val testEnvironment =
          new Environment {
            val store = testStore
            val index = testIndex

            def reportDocumentMetadataMismatch(document: JsObject, meta: DocumentMetadata, validationResults: Seq[JsObject]): Unit = {
              val report = (document, meta, validationResults)
              reports += report
            }
          }

        val newDocumentMetadata =
          Document(id = "article", idField = "title")(
            "title" -> Label
          )

        Helpers.running(testApplication) {
          new Cms(pathPrefix, authenticate,
            environment = testEnvironment,
            documents = Seq(newDocumentMetadata)
          )
        }

        val expectedDocument = obj(
          "id" -> "article_1",
          "header" -> "Article 1"
        )

        val expectedValidationResults = Seq(
          obj(
            "id" -> "label",
            "messageKey" -> "required",
            "message" -> "The field `Title` can not be empty",
            "name" -> "title"
          )
        )

        val expectedReport = (expectedDocument, newDocumentMetadata, expectedValidationResults)

        reports is Seq(expectedReport)
      }

    """|Having invalid documents in the store is no problem as long as we do not 
       |serve them to our application. The simplest thing we can do is provide a 
       |way for the store to make an invalid instances valid.
       |
       |A naive way to do this is to just create a store that transforms the 
       |document to the new structure. For simple evolutions this works, but for 
       |more complex evolutions problems arise. Consider a document that starts
       |out with a `header` property, this property then changes to `title`. 
       |At a later time the `header` property is reintroduced, the document now
       |should have both a `header` and a `title` property.
       |
       |In the above example it's quite tricky to determine what transformations 
       |need to be applied from the document itself. For this reason we introduced
       |version numbers and explicit evolutions that will be applied.""".stripMargin - {

      val reports = ListBuffer.empty[(JsObject, DocumentMetadata, Seq[JsObject])]
      val testEnvironment =
        new Environment {
          val store = testStore
          val index = testIndex

          def reportDocumentMetadataMismatch(document: JsObject, meta: DocumentMetadata, validationResults: Seq[JsObject]): Unit = {
            val report = (document, meta, validationResults)
            reports += report
          }
        }

      example {
        def renameHeaderToTitle(document: JsObject): JsObject = {
          val header = (document \ "header").as[JsValue]
          document - "header" + ("title" -> header)
        }

        val newDocumentMetadata =
          Document(id = "article", idField = "title")(
            "title" -> Label
          ) withEvolutions (
              1 -> renameHeaderToTitle
            )

        Helpers.running(testApplication) {
          new Cms(pathPrefix, authenticate,
            environment = testEnvironment,
            documents = Seq(newDocumentMetadata)
          )
        }

        reports is Seq.empty
      }
    }
       
       "update the index" - {}
  }
}