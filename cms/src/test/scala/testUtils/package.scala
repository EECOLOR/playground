import org.qirx.cms.Cms
import org.joda.time.DateTimeUtils
import java.util.concurrent.atomic.AtomicInteger

package object testUtils {
  val cmsName = classOf[Cms].getSimpleName
  
  def withFixedDateTime[T](code: => T): T = this.synchronized {
    DateTimeUtils.setCurrentMillisFixed(1310323161323L)
    try code
    finally DateTimeUtils.setCurrentMillisSystem()
  }

}