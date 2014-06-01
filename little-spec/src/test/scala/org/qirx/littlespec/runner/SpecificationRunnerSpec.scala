// format: +preserveDanglingCloseParenthesis
package org.qirx.littlespec.runner

import org.qirx.littlespec.Specification
import scala.concurrent.duration._
import scala.collection.mutable
import org.qirx.littlespec.Fragment
import org.qirx.littlespec.Success
import org.qirx.littlespec.Result
import org.qirx.littlespec.CompoundResult
import org.qirx.littlespec.Text

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

      results is Seq(Success(Text("example"))(0.millis))
    }

    "correctly execute nested examples" - {
      val results = runner.run(
        new Specification {
          "examples" - {
            "example" - success
          }
        }
      )

      results is Seq(CompoundResult(Text("examples"), Seq(Success(Text("example"))(0.millis))))
    }
  }
}