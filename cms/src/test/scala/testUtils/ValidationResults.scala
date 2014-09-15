package testUtils

import play.api.libs.json.Json.obj

trait ValidationResults {

  def messageResult(id: String, problem:String) =
    obj(
      "id" -> id,
      "messageKey" -> problem,
      "message" -> s"testProperty.$problem"
    )
    
    def requiredResult(id: String) =
    messageResult(id, "required")

  def invalidTypeResult(id: String) =
    obj(
      "id" -> id,
      "error" -> "invalidType"
    )
    
}