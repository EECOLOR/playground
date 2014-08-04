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
import org.qirx.cms.construction.Create
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

object _05_Evolution extends Specification with Example {

  """|#Evolution
     |
     |An important part of an application is it's ability to evolve. This means 
     |that your domain model will change. Evolution of the model is a tricky 
     |subject because documents have been stored with a previous version of the 
     |model.
     |
     |There are different ways to solve the problems associated with this fact.
     |Here I will explain the choices I made, please raise an issue if you have 
     |a good idea about one of these.
     |
     |The metadata is available as Scala code. This means we have no way of 
     |detecting if it has changed. We could theoretically use macro's to store 
     |metadata at compile time and detect when it has changed. The problem is 
     |that we can not make a distinction between compilation for development and 
     |compilation for production.
     |
     |So in order to detect change we check and store the metadata at runtime, 
     |as soon as an instance of the `$cmsName` is being created.""".stripMargin - {

    val pathPrefix = "/api"
    val authenticate: RequestHeader => Future[Boolean] = { _ => Future.successful(true) }
    val testStore = new TestStore
    val testApplication = TestApplication.fakeApplication()

    """|##Invalid documents
       |
       |The first problem arises when the new metadata makes previously saved 
       |documents invalid. Since we are checking stuff runtime we need some way 
       |to report this problem. Of course we need to try and prevent that from 
       |happening, but since we are human, we need to get notified in case we
       |make a mistake. More on preventing mistakes in later in this document.
       |
       |Note that a method in the `Environment` is used to report the error.
       |""".stripMargin -
      example {
        val reports = ListBuffer.empty[(JsObject, DocumentMetadata, Seq[JsObject])]
        val testEnvironment =
          new Environment {
            val store = testStore

            def reportDocumentMetadataMismatch(document: JsObject, meta: DocumentMetadata, validationResults: Seq[JsObject]): Unit = {
              val report = (document, meta, validationResults)
              reports += report
            }
          }

        def createPreviousDocument(document: JsObject) = {
          Await.result(testStore(Create("article", "article1", document)), 1.second)
          document
        }

        val previousDocument = createPreviousDocument(
          obj("header" -> "article1", "id" -> "article1"))

        val newDocumentMetadata = Document(id = "article", idField = "title")(
          "title" -> Label
        )

        Helpers.running(testApplication) {
          new Cms(pathPrefix, authenticate,
            environment = testEnvironment,
            documents = Seq(newDocumentMetadata)
          )
        }

        val expectedValidationResults = Seq(
          obj(
            "id" -> "label",
            "messageKey" -> "required",
            "message" -> "The field `Title` can not be empty",
            "name" -> "title"
          )
        )

        val expectedReport = (previousDocument, newDocumentMetadata, expectedValidationResults)

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
       |version numbers.""".stripMargin - {

      val reports = ListBuffer.empty[(JsObject, DocumentMetadata, Seq[JsObject])]
      val testEnvironment =
        new Environment {
          val store = testStore

          def reportDocumentMetadataMismatch(document: JsObject, meta: DocumentMetadata, validationResults: Seq[JsObject]): Unit = {
            val report = (document, meta, validationResults)
            reports += report
          }
        }

      def createPreviousDocument(document: JsObject) = {
        Await.result(testStore(Create("article", "article1", document)), 1.second)
        document
      }

      example {
        val previousDocument = createPreviousDocument(
          obj("header" -> "article1", "id" -> "article1"))

        val newDocumentMetadata = Document(id = "article", idField = "title")(
          "title" -> Label
        ) withEvolutions (
            1 -> (__.json update (__ \ "title").json.copyFrom((__ \ "header").json.pick) andThen (__ \ "header").json.prune)
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

    "test helper" - {}

  }
}