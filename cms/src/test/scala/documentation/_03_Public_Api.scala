package documentation

import org.qirx.littlespec.Specification
import testUtils.ApiExampleSpecification
import testUtils.GetFromApplication

class _03_Public_Api extends Specification with ApiExampleSpecification {
  "# The public API" - {

    val publicApiPrefix = "/api/public"

    withApiExampleIntroduction(apiPrefix = publicApiPrefix) { app =>

      val GET = new GetFromApplication(app, publicApiPrefix)

      s"""|Note that this API provides all of the GET endpoints that are available
          |in the private API without authentication.
          |
          |${moreInformation[_03_Public_Api]}""".stripMargin - {
            pending("Find a way to run all of the /private GET related tests using /public")
          }
      
    }
  }
}