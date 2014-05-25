package org.qirx.yaml

import org.qirx.littlespec.Specification

object RawYamlParserSpec extends Specification {
  import RawYamlParser._

  implicit class StringEnhancements(s: String) {
    private def format(s: String) =
      s.replaceAll(" ", "·")
        .replaceAll("\t", "→")
        .replaceAll("(\r\n|\r|\n)", "↓\n")

    def shouldParseAs(expected: Seq[_]): FragmentBody = {
      parse(s) isLike {
        case Success(result, _) =>
          result is expected withMessage { original =>
            s"${format(s)} is not parsed as ${format(expected)} but as ${format(result)}"
          }
      }
    }

    def strip = s.stripMargin

    def shouldParseAsErrorAt[T](line: Int, column: Int): FragmentBody =
      parse(s) isLike {
        case Error(_, next) =>
          (next.pos.line, next.pos.column) is (line, column) withMessage { original =>
            s"'$s' did not give an error at the correct location: $original"
          }
        case noError => failure(s"'$s' was not parsed as an error: $noError")
      }

  }

  def document(content: String, directives: Seq[Directive] = Seq.empty) =
    Seq(Document(Scalar(content), directives))

  "RawYamlParser should" - {

    val oneDocument = document("Doc")

    "handle empty streams" - {
      val emptyStreams =
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

      def forAllLineEndings(s: String): Seq[String] =
        Seq("\r", "\r\n").foldLeft(Seq(s)) { (seq, replacement) =>
          seq :+ s.replaceAll("\n", replacement)
        }

      for {
        stream <- emptyStreams
        withLineEndings <- forAllLineEndings(stream)
      } {
        val escapeLineEndingCharacters = (message: String) =>
          "Failure for '" +
            withLineEndings
            .replaceAll("\r", "\\\\r")
            .replaceAll("\n", "\\\\n") + "': " + message

        withLineEndings shouldParseAs Seq.empty withMessage escapeLineEndingCharacters
      }

      success
    }

    val twoDocuments = document("Doc1") ++ document("Doc2")

    "handle 'bare' documents" - {

      "Doc" shouldParseAs oneDocument
      "Doc # Comment" shouldParseAs oneDocument

      """|Doc1
         |...
         |Doc2""".strip shouldParseAs twoDocuments

      """|Doc1     # Comment
         |...
         |# No document 1
         |...
         |Doc2
         |...
         |# No document 2""".strip shouldParseAs twoDocuments

      """|Doc1
         |%""".strip shouldParseAs document("Doc1\n%")
    }

    def emptyDocument(directives: Directive*) =
      Seq(Document(Empty, directives))

    "handle 'explicit' documents" - {
      val oneEmptyDocument = emptyDocument()

      "---" shouldParseAs oneEmptyDocument
      "--- " shouldParseAs oneEmptyDocument
      "---\n" shouldParseAs oneEmptyDocument
      """|---
         |# Comment""".strip shouldParseAs oneEmptyDocument

      """|---
         |Doc""".strip shouldParseAs oneDocument

      """|Doc1
         |---
         |Doc2""".strip shouldParseAs twoDocuments

      """|Doc
         |---
         |# No document""".strip shouldParseAs (oneDocument ++ oneEmptyDocument)

      """|---
         |Doc
         |...
         |---
         |# Empty
         |...""".strip shouldParseAs (oneDocument ++ oneEmptyDocument)
    }

    "handle 'directive' documents" - {

      "invalid directives" - {
        "%" shouldParseAsErrorAt (1, 2)
        "% " shouldParseAsErrorAt (1, 2)
        "%A" shouldParseAsErrorAt (1, 3)
        "%A\n" shouldParseAsErrorAt (2, 1)
      }

      "unknown directive" - {

        def unknownDirective(parameters: String*) =
          namedUnknownDirective("Unknown", parameters)
        def namedUnknownDirective(name: String, parameters: Seq[String] = Seq.empty) =
          emptyDocument(UnknownDirective(name, parameters))

        "%Unknown\n---" shouldParseAs unknownDirective()
        "%Unknown p1\n---" shouldParseAs unknownDirective("p1")
        "%Unknown p1 p2\n---" shouldParseAs unknownDirective("p1", "p2")
        "%TAG\n---" shouldParseAs namedUnknownDirective("TAG")
        "%TAGS\n---" shouldParseAs namedUnknownDirective("TAGS")
        "%YAML\n---" shouldParseAs namedUnknownDirective("YAML")
        "%YAMLS\n---" shouldParseAs namedUnknownDirective("YAMLS")
      }

      "tag directive" - {

        def emptyWith(tagHandle: TagHandle, prefix: TagPrefix) =
          emptyDocument(TagDirective(tagHandle, prefix))

        "%TAG \n---" shouldParseAsErrorAt (1, 6)

        def testWithHandle(h: String, handle: TagHandle) = {
          val pos = h.length
          s"%TAG $h\n---" shouldParseAsErrorAt (1, 6 + pos)
          s"%TAG $h \n---" shouldParseAsErrorAt (1, 7 + pos)
          s"%TAG $h !\n---" shouldParseAs emptyWith(handle, LocalTagPrefix(""))
          s"%TAG $h !test\n---" shouldParseAs emptyWith(handle, LocalTagPrefix("test"))
          s"%TAG $h !%0\n---" shouldParseAsErrorAt (1, 8 + pos)
          s"%TAG $h !%a\n---" shouldParseAsErrorAt (1, 8 + pos)
          s"%TAG $h !%0x\n---" shouldParseAsErrorAt (1, 8 + pos)
          s"%TAG $h !%ax\n---" shouldParseAsErrorAt (1, 8 + pos)
          s"%TAG $h !%x0\n---" shouldParseAsErrorAt (1, 8 + pos)
          s"%TAG $h !%xa\n---" shouldParseAsErrorAt (1, 8 + pos)
          s"%TAG $h !%00\n---" shouldParseAs emptyWith(handle, LocalTagPrefix("%00"))
          s"%TAG $h !%0a\n---" shouldParseAs emptyWith(handle, LocalTagPrefix("%0a"))
          s"%TAG $h !%a0\n---" shouldParseAs emptyWith(handle, LocalTagPrefix("%a0"))
          s"%TAG $h !%aa\n---" shouldParseAs emptyWith(handle, LocalTagPrefix("%aa"))
          s"%TAG $h ,\n---" shouldParseAsErrorAt (1, 7 + pos)
          s"%TAG $h [\n---" shouldParseAsErrorAt (1, 7 + pos)
          s"%TAG $h ]\n---" shouldParseAsErrorAt (1, 7 + pos)
          s"%TAG $h {\n---" shouldParseAsErrorAt (1, 7 + pos)
          s"%TAG $h }\n---" shouldParseAsErrorAt (1, 7 + pos)
          s"%TAG $h test\n---" shouldParseAs emptyWith(handle, GlobalTagPrefix("test"))
          s"%TAG $h 10\n---" shouldParseAs emptyWith(handle, GlobalTagPrefix("10"))
          s"%TAG $h a[b]c,d!e\n---" shouldParseAs emptyWith(handle, GlobalTagPrefix("a[b]c,d!e"))
        }

        "primary tag handle" - testWithHandle("!", PrimaryTagHandle)
        "secondary tag handle" - testWithHandle("!!", SecondaryTagHandle)
        "named tag handle" - testWithHandle("!test!", NamedTagHandle("test"))
      }

      "YAML directive" - {
        "%YAML \n---" shouldParseAsErrorAt (1, 7)
        "%YAML 1\n---" shouldParseAsErrorAt (1, 8)
        "%YAML 1.\n---" shouldParseAsErrorAt (1, 9)
        "%YAML 0.0\n---" shouldParseAs emptyDocument(YamlDirective("0.0"))
        "%YAML 10.10\n---" shouldParseAs emptyDocument(YamlDirective("10.10"))
      }
    }

    "mixed documents" - {
      """|Doc1
         |---
         |# Empty
         |...
         |%YAML 1.2
         |---
         |Doc2""".strip shouldParseAs
        (document("Doc1") :+
          Document.empty) ++
          document("Doc2", Seq(YamlDirective("1.2")))
    }

    "handle litteral scalars" - {

      "|\nDoc" shouldParseAs oneDocument
      "|\n\nDoc" shouldParseAs document("\nDoc")
      "| #Comment\n\nDoc" shouldParseAs document("\nDoc")
      """||
         | literal
         |  text
         |""".strip shouldParseAs document("literal\n text\n")
      s"""||
          |${' '}
          | literal # Comment
          |""".strip shouldParseAs document("\nliteral\n")
      """||
         | # Not a comment
         |  text""".strip shouldParseAs document("# Not a comment\n text")
      s"""||
          | ${'\t'}
          |  text""".strip shouldParseAs document("\t\n text")
      """||1
         |  text""".strip shouldParseAs document(" text")
      """||1
         |  # Comment
         |  text""".strip shouldParseAs document("\n text")
    }

    "handle folded scalars" - {
      s"""|>
          |  trimmed
          |${"  "}
          |${' '}
          |
          |  as
          |  space""".strip shouldParseAs document("trimmed\n\n\nas space")

      """|>
         |folded
         |text
         |""".strip shouldParseAs document("folded text\n")

      s"""|>
          | folded
          | text
          |  not
          |  folded
          |""".strip shouldParseAs document("folded text\n not\n folded\n")

      s"""|>
          |  foo${' '}
          |${' '}
          |  ${'\t'} bar
          |
          |  baz
          |""".strip shouldParseAs document("foo \n\n\t bar\n\nbaz\n")

      """|>
         |
         | folded
         | line
         |
         | next
         | line
         |   * bullet
         |
         |   * list
         |   * lines
         |
         | last
         | line
         |""".strip shouldParseAs document("\nfolded line\nnext line\n  * bullet\n\n  * list\n  * lines\n\nlast line\n")

      s"""|>
          | ${'\t'}
          | detected
          |""".strip shouldParseAs document("\t detected\n") withMessage "This is as far as I can see a bug in the specification"
    }
  }

  "report nice errors" - {

  }
  "do we need these extensive tests?" - {
    implicit class IntEnhancements(i: Int) {
      def ->(j: Int) = Seq.range(i, j).toSet
    }

    val allChars = (0 -> Char.MaxValue.toInt)

    "deal correctly with invalid characters" - {

      val `C0 control block` = (0x0 -> 0x1F) - 0x9 - 0xA - 0xD + 0x7F
      val `C1 control block` = (0x80 -> 0x97) - 0x85
      val `surrogate block` = (0xD800 -> 0xDFFF) + 0xFFFE + 0xFFFF
      val excludedChars = `C0 control block` ++ `C1 control block` ++ `surrogate block`
      val allowedChars = allChars -- excludedChars

      def expectCharacterNotAllowedFor(i: Int, column: Int): PartialFunction[ParseResult[_], FragmentBody] = {
        case Error(message, rest) =>
          message is "Character not allowed: 0x" + i.toHexString
          rest.pos.column is column
      }

      "fail for characters outside of the printable subset of the Unicode character set" - {

        //for (i <- excludedChars)
        //parse(i.toChar.toString) isLike
        //expectCharacterNotAllowedFor(i, column = 1)

        "that are not the first character" - {
          //for (i <- excludedChars)
          //parse("_" + i.toChar.toString) isLike
          //expectCharacterNotAllowedFor(i, column = 2)

          pending("disabled for performance")
        }
      }

      "succeed for all other characters" - {
        //for (i <- allowedChars)
        //parse(" " + i.toChar.toString) isLike {
        //case Success(_, _) => success
        //}

        pending("disabled for performance")
      }
    }
    "invalid prefix characters" - {
      def tag(handle: String, prefix: String) =
        s"%TAG $handle $prefix\n---"

      def primary(prefix: String) = tag("!", prefix)

      val decDigitChars = (0x30 -> 0x39)
      val asciiLetterChars = (0x41 -> 0x5A) ++ (0x61 -> 0x7A)
      val wordChars = decDigitChars ++ asciiLetterChars + '-'.toInt
      val uriChars = wordChars + '#' +
        ';' + '/' + '?' + ':' + '@' + '&' + '=' + '+' + '$' + ',' +
        '_' + '.' + '!' + '~' + '*' + '\'' + '(' + ')' + '[' + ']'

      val invalidChars = allChars -- uriChars - '%' - '\n' - '\r'
      //for (i <- invalidChars)
      //primary("!" + i.toChar.toString) shouldParseAsErrorAt (1, 9)

      pending("disabled for performance")
    }
  }
}