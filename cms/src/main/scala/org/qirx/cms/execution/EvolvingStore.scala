package org.qirx.cms.execution

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

import org.qirx.cms.construction.Delete
import org.qirx.cms.construction.Get
import org.qirx.cms.construction.List
import org.qirx.cms.construction.Save
import org.qirx.cms.construction.SaveIdReference
import org.qirx.cms.construction.Store
import org.qirx.cms.machinery.~>

import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import org.qirx.cms.metadata.Evolutions

/**
 * A store that wraps another store. It will add a version to the saved documents 
 * which is used (during retrieval) to determine which evolutions should be applied.
 * After the evolutions have been applied the version is removed from the retrieved 
 * documents.
 */
class EvolvingStore(
  store: Store ~> Future,
  evolutions: Map[String, Evolutions])(
    implicit ec: ExecutionContext) extends (Store ~> Future) {

  private val VERSION = "_version"

  def transform[x] = {
    case List(metaId, fieldSet) =>
      val list = List(metaId, addVersion(fieldSet))
      store(list).map(_ map transformDocumentFor(metaId))

    case Get(metaId, id, fieldSet) =>
      val get = Get(metaId, id, addVersion(fieldSet))
      store(get).map(_ map transformDocumentFor(metaId))

    case Save(metaId, id, document) =>
      val version = obj(VERSION -> latestVersionFor(metaId))
      store(Save(metaId, id, document ++ version))

    case saveIdReference: SaveIdReference =>
      store(saveIdReference)

    case delete: Delete =>
      store(delete)
  }

  private def addVersion(fieldSet: Set[String]) =
    if (fieldSet.isEmpty) fieldSet
    else fieldSet + VERSION

  private def evolutionsFor(metaId: String) = evolutions.get(metaId)

  private def transformDocumentFor(metaId: String) =
    applyEvolutionsFor(metaId) andThen removeVersion

  private def applyEvolutionsFor(metaId: String): JsObject => JsObject = { document =>
    val documentVersion = (document \ VERSION).as[Int]
    evolutionsFor(metaId).fold(document)(_.applyEvolutions(document, documentVersion))
  }

  private def latestVersionFor(metaId: String) =
    evolutionsFor(metaId).map(_.latestVersion).getOrElse(0)

  private val removeVersion: JsObject => JsObject = _ - VERSION
}
