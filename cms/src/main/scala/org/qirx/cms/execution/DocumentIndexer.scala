package org.qirx.cms.execution

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.qirx.cms.construction.Index
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.api.ExtractId
import org.qirx.cms.machinery.BuildTools
import org.qirx.cms.machinery.ExecutionTools
import org.qirx.cms.machinery.ProgramType
import org.qirx.cms.machinery.~>
import org.qirx.cms.metadata.DocumentMetadata

class DocumentIndexer(
    documents:Seq[DocumentMetadata], 
    store: Store ~> Future,
    index:Index ~> Future)(implicit ec: ExecutionContext) {

  import BuildTools._
  import ExecutionTools._
  
  def index():Future[Seq[Unit]] = indexProgram.foldMap(runner)
  
  private type Elements = ProgramType[(Base + Store + Index + Seq)#T]

  private def indexProgram(implicit e: Elements) =
    for {
      meta <- documents.asProgram
      _ <- Index.Delete(meta.id) 
      documents <- Store.List(meta.id, Set.empty)
      document <- documents.asProgram
      id <- ExtractId(document)
      _ <- Index.Put(meta.id, id, document)
    } yield ()

  private val runner = {
    val seqRunner = SeqToFutureSeq
    val storeRunner = store andThen FutureToFutureSeq
    val indexRunner = index andThen FutureToFutureSeq
    val systemRunner = SystemToId andThen IdToFuture andThen FutureToFutureSeq

    seqRunner or storeRunner or indexRunner or systemRunner
  }

}