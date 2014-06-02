// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec.runner

import org.qirx.littlespec.Specification
import scala.concurrent.duration._
import scala.collection.mutable
import org.qirx.littlespec.fragments.Fragment
import org.qirx.littlespec.fragments.Success
import org.qirx.littlespec.fragments.Result
import org.qirx.littlespec.fragments.CompoundResult
import org.qirx.littlespec.fragments.Text

object SpecificationRunnerSpec extends Specification {

  val runner = new DefaultSpecificationRunner

  "DefaultSpecificationRunner runner should" - {

    "handle emtpy specifications" - {
      val results = runner.run(new Specification {})
      results is Seq.empty
    }

    "correctly execute single example" - {
      val results = runner.run(
        new Specification {
          "example" - success
        }
      )

      results isLike {
        case Seq(Success(Text("example"))) => success
      }
    }

    "correctly execute nested examples" - {
      val results = runner.run(
        new Specification {
          "examples" - {
            "example" - success
          }
        }
      )

      results isLike {
        case Seq(CompoundResult(Text("examples"),
          Seq(Success(Text("example"))))) => success
      }
    }
  }
}