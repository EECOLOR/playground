package org.qirx.yaml

import org.qirx.littlespec.Specification
import org.qirx.yaml.model.Yaml
import org.qirx.yaml.model.Document
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.CharSequenceReader

object ParserSpec extends Specification {

  object Parser extends RegexParsers {
    val comment = "#.*".r
    val documentPrefix = comment.*

    val documentLine = "(?!\\.\\.\\.).+".r
    val documentContent = documentLine.+ ^^ { _.mkString("\n") }
    val documentSuffix = "..."
    val document = documentPrefix ~ documentContent.? <~ documentSuffix.? ^? {
      case p ~ Some(d) => Document(d, p)
    }
    val yaml = document.* ^^ (Yaml apply _)

    def parse(stream: String): ParseResult[Yaml] =
      parse(yaml, stream)
  }

  def parse(stream: String) =
    Parser.parse(stream)

  "Parser should accept" - {

    "an empty stream" - {
      "" isParsedTo yaml
    }

    "a document prefix - no documents" - {
      "# A Prefix" isParsedTo yaml
    }

    "a bare document" - {
      "Document" isParsedTo document("Document")

      "with prefix" - {
        "# A Prefix\nDocument" isParsedTo document("# A Prefix", "Document")
      }

      "with multiline prefix" - {
        "# P1\n# P2\nDocument" isParsedTo
          yaml(Document("Document", Seq("# P1", "# P2")))
      }
    }

    "multiple bare documents" - {
      """|Bare
         |document
         |...
         |# No document
         |...
         ||
         |%!PS-Adobe-2.0 # Not the first line
         |""".stripMargin isParsedTo
        Yaml(Seq(Document("Bare\nDocument"), Document("%!PS-Adobe-2.0\n")))

    }
  }

  "Productions should be as defined in the spec" - {
    // Spaces, valid characters, etc.
  }

  // using named arguments to keep the correct property names
  val yaml = Yaml()
  def yaml(document: Document) = Yaml(document = Seq(document))
  def document(content: String) = yaml(Document(content = content))
  def document(prefix: String, content: String) = yaml(Document(content, prefix = Seq(prefix)))

  implicit class IsParsedToEnhancement(s: String) {

    def isParsedTo(expected: Yaml) =
      parse(s) isLike {
        case Parser.Success(result, rest) => result is expected
        case noSuccess: Parser.NoSuccess => failure(noSuccess.msg)
      }
  }
}