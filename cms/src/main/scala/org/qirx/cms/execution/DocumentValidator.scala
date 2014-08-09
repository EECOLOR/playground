package org.qirx.cms.execution

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import org.qirx.cms.Environment
import org.qirx.cms.construction.GetMessages
import org.qirx.cms.construction.Metadata
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.Store.List
import org.qirx.cms.construction.Validate
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.machinery.Id
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata
import play.api.libs.json.JsObject
/**
 * The validate method validates all documents and returns the results as 
 * a seq of tuples.
 */
class DocumentValidator(
  documents: Seq[DocumentMetadata],
  metadata: Metadata ~> Id,
  store: Store ~> Future)(implicit val ec: ExecutionContext) 
  extends BuildTools with ExecutionTools {

  type Document = JsObject
  type ValidationResult = Seq[JsObject]
  type Result = (Document, DocumentMetadata, ValidationResult)
  
  def validate():Future[Seq[Result]] = validationProgram.foldMap(runner)
  
  private type Elements = ProgramType[(Base + Store + Metadata + Seq)#T]

  private def validationProgram(implicit e: Elements) =
    for {
      meta <- documents.asProgram
      messages <- GetMessages(meta)
      documents <- List(meta.id, Set.empty)
      document <- documents.asProgram
      result <- Validate(meta, document, Set.empty, messages)
    } yield (document, meta, result)

  private val runner = {
    val seqRunner = SeqToFutureSeq
    val metadataRunner = metadata andThen IdToFuture andThen FutureToFutureSeq
    val storeRunner = store andThen FutureToFutureSeq
    val systemRunner = SystemRunner andThen IdToFuture andThen FutureToFutureSeq

    seqRunner or metadataRunner or storeRunner or systemRunner
  }
}