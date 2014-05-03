package org.qirx.yaml

import org.qirx.littlespec.Specification
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.input.CharArrayReader

object RawYamlParserSpec extends Specification {
  import RawYamlParser._

  implicit class StringEnhancements(s: String) {
    def isParsedAs(expected: Seq[_]): FragmentBody =
      parse(s.stripMargin) isLike {
        case Success(result, _) => result is expected
      }
  }

  "RawYamlParser should" - {

    "deal correctly with invalid characters" - {

      implicit class IntEnhancements(i: Int) {
        def ->(j: Int) = Seq.range(i, j).toSet
      }

      val `C0 control block` = (0x0 -> 0x1F) - 0x9 - 0xA - 0xD + 0x7F
      val `C1 control block` = (0x80 -> 0x97) - 0x85
      val `surrogate block` = (0xD800 -> 0xDFFF) + 0xFFFE + 0xFFFF
      val excludedChars = `C0 control block` ++ `C1 control block` ++ `surrogate block`

      def expectCharacterNotAllowedFor(i: Int, column: Int): PartialFunction[ParseResult[_], FragmentBody] = {
        case Error(message, rest) =>
          message is "Character not allowed: 0x" + i.toHexString
          rest.pos.column is column
      }

      "fail for characters outside of the printable subset of the Unicode character set" - {

        for (i <- excludedChars)
          parse(i.toChar.toString) isLike
            expectCharacterNotAllowedFor(i, column = 1)

        "that are not the first character" - {
          for (i <- excludedChars)
            parse("_" + i.toChar.toString) isLike
              expectCharacterNotAllowedFor(i, column = 2)

          success
        }
      }
    }

    "handle empty streams" - {
      Seq(
        "",
        "\n",
        "...",
        "...\n",
        "\n...",
        "\n...\n",
        " ",
        " \n",
        "\n ",
        "\n \n",
        " \n...",
        "... ",
        "...\t",
        "\t",
        "#",
        "# ",
        "# Comment",
        "# Comment 1\n# Comment 2",
        " #",
        " # ",
        " # Comment",
        "... #",
        "  # Comment\n   \n",
        "... # Comment \n...")
        .foreach(_ isParsedAs Seq.empty)

      success
    }

    val oneDocument = Seq(Document("Doc"))
    val twoDocuments = Seq(Document("Doc1"), Document("Doc2"))

    "handle 'bare documents'" - {

      "content only" - {
        "Doc" isParsedAs oneDocument
      }
      "with comments" - {
        "Doc # Comment" isParsedAs oneDocument
      }
      "multiple" - {
        """|Doc1
           |...
           |Doc2""" isParsedAs twoDocuments
      }
      "mixed with 'non-documents'" - {
        """|Doc1     # Comment
           |...
           |# No document 1
           |...
           |Doc2
           |...
           |# No document 2""" isParsedAs twoDocuments
      }
    }

    "handle 'explicit' documents" - {
      val oneEmptyDocument = Seq(Document(None))
      "empty" - {
        "---" isParsedAs oneEmptyDocument
        "--- " isParsedAs oneEmptyDocument
        "---\n" isParsedAs oneEmptyDocument
        "---\n# Comment" isParsedAs oneEmptyDocument
      }
      "with content" - {
        """|---
           |Doc""" isParsedAs oneDocument
      }
      "multiple" - {
        """|Doc1
           |---
           |Doc2""" isParsedAs twoDocuments
      }
      "mixed with 'non-documents'" - {
        """|Doc
           |---
           |# No document""" isParsedAs (oneDocument ++ oneEmptyDocument)
      }
      "mixed with document end markers" - {
        """|---
           |Doc
           |...
           |---
           |# Empty
           |...""" isParsedAs (oneDocument ++ oneEmptyDocument)
      }
    }
  }
}