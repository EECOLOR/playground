package spec

import org.qirx.littlespec.Specification

object StructureSpec extends Specification {

  "An empty specification is written like this" - {
    // nothing
  }

  "Specifications can be nested" - {
      "specification1" - {

      }
      "specification2" - {

      }
  }

}