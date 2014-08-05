package org.qirx.cms.execution

import scala.concurrent.Future
import org.qirx.cms.construction.Store
import org.qirx.cms.construction.List
import org.qirx.cms.construction.Get
import org.qirx.cms.construction.Save
import org.qirx.cms.construction.SaveIdReference
import org.qirx.cms.construction.Delete
import org.qirx.cms.metadata.Evolution
import org.qirx.cms.machinery.~>
import play.api.libs.json.Json.obj
import play.api.libs.json.JsObject
import scala.concurrent.ExecutionContext

class VersionedStore(store: Store ~> Future, evolutions: Map[String, Seq[Evolution]])(implicit ec: ExecutionContext) extends (Store ~> Future) {
  val VERSION = "_version"

  def latestVersion(metaId: String) =
    evolutions.get(metaId).map { evolutions =>
      if (evolutions.isEmpty) 0
      else evolutions.map { case (version, _) => version }.max
    }.getOrElse(0)

  def transform[x] = {
    case list @ List(metaId, fieldSet) =>
      val fieldSetWithVersion = 
        if (fieldSet.isEmpty) fieldSet
        else fieldSet + VERSION
      store(List(metaId, fieldSetWithVersion)).map(_.map(applyEvolutions(metaId) andThen removeVersion))

    case get @ Get(metaId, id, fieldSet) =>
      val fieldSetWithVersion = 
        if (fieldSet.isEmpty) fieldSet
        else fieldSet + VERSION
      store(Get(metaId, id, fieldSetWithVersion)).map(_.map(applyEvolutions(metaId) andThen removeVersion))

    case Save(metaId, id, document) =>
      println("save", document ++ obj(VERSION -> latestVersion(metaId)))
      store(Save(metaId, id, document ++ obj(VERSION -> latestVersion(metaId))))

    case saveIdReference: SaveIdReference =>
      store(saveIdReference)

    case delete: Delete =>
      store(delete)
  }

  def applyEvolutions(metaId: String): JsObject => JsObject = { document =>
    println("applyEvolutions", document)
    val documentVersion = (document \ VERSION).as[Int]
    val evolutions = this.evolutions.get(metaId).getOrElse(Seq.empty)
    val applicableTransformations = evolutions.collect {
      case (version, transformation) if version > documentVersion => transformation
    }
    applicableTransformations.foldLeft(document) { (document, transform) =>
      transform(document)
    }
  }
  val removeVersion: JsObject => JsObject = _ - VERSION
}
