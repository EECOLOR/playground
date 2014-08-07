package testUtils

import org.qirx.littlespec.macros.Location
import org.qirx.littlespec.Specification
import org.qirx.littlespec.io.Source
import org.qirx.littlespec.fragments.Title
import org.qirx.littlespec.fragments.Text
import org.qirx.littlespec.fragments.Fragment
import scala.reflect.ClassTag
import org.qirx.littlespec.reporter.MarkdownReporter

trait Example { self: Specification =>

  class CodeString[T](val value:T, code:String) {
    override def toString = code
  }
  
  def codeString[T](code: => T)(implicit location:Location) = 
    new CodeString(code, Source.codeAtLocation(location).text)
  
  class ExampleContainer(implicit location: Location) { self =>

    var parts: Seq[Title] = Seq(Source.codeAtLocation(location))

    class Sub(part: Title) {
      def code[T <: ExampleContainer](code: self.type => T) =
        code(self).withPreviousParts[T](parts :+ part)
    }
    def text(part: String) = new Sub(Text(part))

    def withPreviousParts[T >: this.type](previousParts: Seq[Title]):T = {
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
  
  def link[T : ClassTag] = {
    val fullyQualifiedName = implicitly[ClassTag[T]].runtimeClass.getName

    val name = MarkdownReporter.name(fullyQualifiedName)
    val fileName = MarkdownReporter.fileName(fullyQualifiedName)

    val cleanFileName = fileName.replaceAll("\\$", "")
    val cleanName = name.replaceAll("_", " ").replaceAll("\\$", "").trim

    s"[$cleanName]($cleanFileName)"
  }

  def moreInformation[T : ClassTag] =
    s"For detailed information see ${link[T]}"
}