package testUtils

import org.qirx.littlespec.macros.Location
import org.qirx.littlespec.Specification
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.fragments.Title
import org.qirx.littlespec.fragments.Text
import org.qirx.littlespec.fragments.Fragment
import scala.reflect.ClassTag
import org.qirx.littlespec.reporter.MarkdownReporter
import org.qirx.littlespec.fragments.Code

trait Example { self: Specification =>

  class ExampleContainer(implicit location: Location) { self =>

    var parts: Seq[Title] = Seq(codeWithReplacements)

    class Sub(part: Title) {
      def code[T <: ExampleContainer](code: self.type => T) =
        code(self).withPreviousParts[T](parts :+ part)
    }
    def text(part: String) = new Sub(Text(part))

    def withPreviousParts[T >: this.type](previousParts: Seq[Title]): T = {
      parts = previousParts ++ self.parts
      this
    }

    def withSpecification(body: self.type => FragmentBody) = {
      parts.foreach(createFragment(_, success))
      body(self)
    }
  }

  class Example(implicit location: Location) {

    def withSpecification(body: this.type => FragmentBody) =
      createFragment(Source.codeAtLocation(location), body(this))
  }

  def exampleWithReplacements[T](code: => T)(implicit asBody: T => Fragment.Body, location: Location): Fragment =
    createFragment(codeWithReplacements, code)
}