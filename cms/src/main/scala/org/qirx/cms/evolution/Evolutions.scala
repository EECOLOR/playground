package org.qirx.cms.evolution

import play.api.libs.json.JsObject

/**
 * Wrapper for a sequence of evolutions. Provides methods to get the latest 
 * version and to apply the evolutions to a given document.
 */
class Evolutions(evolutions: Seq[Evolution]) {

  private val version: Evolution => Int = { case (version, _) => version }
  
  private val sortedEvolutions = evolutions.sortBy(version)

  val latestVersion = sortedEvolutions.lastOption.map(version).getOrElse(0)

  def applyEvolutions(document: JsObject, documentVersion: Int): JsObject = {
    val applicableTransformations =
      sortedEvolutions.collect {
        case (version, transformation) if version > documentVersion => transformation
      }
    applicableTransformations.foldLeft(document)(applyTransformation)
  }

  def withEvolutions(evolutions: Seq[Evolution]) =
    new Evolutions(this.evolutions ++ evolutions)

  private def applyTransformation(document: JsObject, transformation: Transformation) =
    transformation(document)
}