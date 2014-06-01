import org.qirx.littlespec.Fragment
import org.qirx.littlespec.assertion.BasicAssertEnhancements
import org.qirx.littlespec.assertion.StaticAssertions
import org.qirx.littlespec.assertion.ThrowingAssertions

package object testUtils extends StaticAssertions with ThrowingAssertions with BasicAssertEnhancements {

  val beAFailure = throwA[Fragment.ThrowableFailure]
  def beAFailureWithMessage(expected: String) =
    beAFailure.like {
      case Fragment.ThrowableFailure(message) => message is expected
      case _ => failure("no exception was thrown")
    }
}