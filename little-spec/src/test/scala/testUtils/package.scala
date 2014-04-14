import org.qirx.littlespec.Assertion
import org.qirx.littlespec.Fragment
import org.qirx.littlespec.assertion.ThrowingAssertions
import org.qirx.littlespec.assertion.StaticAssertions

package object testUtils extends StaticAssertions with ThrowingAssertions {

  val beAFailure = throwA[Fragment.ThrowableFailure]
}