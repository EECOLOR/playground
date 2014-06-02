import org.qirx.littlespec.Fragment
import org.qirx.littlespec.assertion.BasicAssertEnhancements
import org.qirx.littlespec.assertion.StaticAssertions
import org.qirx.littlespec.assertion.ThrowingAssertions

package object testUtils extends StaticAssertions with ThrowingAssertions with BasicAssertEnhancements {

  val beAFailure = throwA[Fragment.Failure]
  def beAFailureWithMessage(message: String) =
    beAFailure withMessage message

  implicit class FailWith(t: => Fragment.Body) {
    def failsWith(message: String) = t must beAFailureWithMessage(message)
  }
}