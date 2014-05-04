package org.qirx.yaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.language.implicitConversions
import scala.collection.mutable.ListBuffer

object RawYamlParser extends Parsers {

  type Elem = Char

  /* AST */
  case class Document(content: Node, directives: Seq[Directive] = Seq.empty)
  object Document extends (Node => Document) {

    def apply(content: Node): Document =
      this(content, Seq.empty)

    val empty = Document(Empty)
  }

  sealed trait Node
  case object Empty extends Node
  case class Scalar(value: String) extends Node

  sealed trait Directive
  case class UnknownDirective(name: String, parameters: Seq[String] = Seq.empty) extends Directive
  case class TagDirective(handle: TagHandle, prefix: TagPrefix) extends Directive
  case class YamlDirective(version: String) extends Directive

  sealed trait TagHandle
  object PrimaryTagHandle extends TagHandle
  object SecondaryTagHandle extends TagHandle
  case class NamedTagHandle(name: String) extends TagHandle

  sealed trait TagPrefix
  case class LocalTagPrefix(name: String) extends TagPrefix
  case class GlobalTagPrefix(name: String) extends TagPrefix

  /* PARSERS */

  def parse(s: String) =
    try {
      yamlStream(new SkippingReader(new CharSequenceReader(s), anyComment))
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }

  /* COMMENTS */

  lazy val anyComment = commentLine | comment
  lazy val commentLine = startOfLine ~> white.* ~> commentText.? <~ (break | endOfFile)
  lazy val comment = white.+ ~> commentText
  lazy val commentText = '#' ~> nonBreak.*

  /* DOCUMENTS */

  lazy val yamlStream =
    (anyDocument | nonDocument).* ^^ removeNonDocuments

  lazy val nonDocument = documentEnd ^^^ null

  lazy val anyDocument =
    directiveDocument | explicitDocument | bareDocument

  lazy val directiveDocument =
    directive.+ ~ commit(explicitDocument) ^^ {
      case directives ~ document => document.copy(directives = directives)
    }

  lazy val explicitDocument =
    directivesEnd ~> bareDocument.? ^^ {
      case Some(document) => document
      case None => Document(Empty)
    }

  lazy val bareDocument =
    documentContent <~ documentEnd.? ^^ Document

  /* DIRECTIVES */

  lazy val directive = {
    lazy val parser =
      '%' ~> commit((yamlDirective | tagDirective | reservedDirective) <~ break)

    lazy val yamlDirective = {
      lazy val parser =
        "YAML" ~> white.+ ~> commit(yamlVersion) ^^ YamlDirective

      lazy val yamlVersion = decDigitChar.+ ~ ('.' ~> decDigitChar.+)
      parser
    }

    lazy val tagDirective = {
      lazy val parser =
        "TAG" ~> white.+ ~> commit(tagHandle ~ (white.+ ~> tagPrefix)) ^^ TagDirective

      lazy val tagHandle = {
        lazy val parser = namedTagHandle | secondaryTagHandle | primaryTagHandle

        lazy val primaryTagHandle = '!' ^^^ PrimaryTagHandle

        lazy val secondaryTagHandle = "!!" ^^^ SecondaryTagHandle

        lazy val namedTagHandle = '!' ~> wordChar.+ <~ '!' ^^ NamedTagHandle

        parser
      }

      lazy val tagPrefix = {
        lazy val parser = localTagPrefix | globalTagPrefix

        lazy val localTagPrefix = '!' ~> uriChar.* ^^ LocalTagPrefix

        lazy val globalTagPrefix = tagChar ~ uriChar.* ^^ GlobalTagPrefix

        parser
      }

      parser
    }

    lazy val reservedDirective =
      nonWhite.+ ~ (white.+ ~> nonWhite.+).* ^^ UnknownDirective

    parser
  }

  lazy val documentContent =
    scalar

  lazy val scalar =
    documentChar.+ ^^ Scalar

  lazy val documentChar =
    not(forbidden) >> allowedChar

  lazy val forbidden =
    documentEnd | directivesEnd

  /* MARKERS */
  lazy val directivesEnd = marker("---")
  lazy val documentEnd = marker("...")

  private def marker(marker: String) =
    break.? ~ startOfLine ~ marker.p ~ (break | white | endOfFile)

  /* INDICATORS */
  lazy val startOfLine = whenInput(_.pos.column == 1, "Expected start of line")
  lazy val endOfFile = whenInput(_.atEnd, "Expected end of file")

  /* CHARS */

  lazy val break = (cariageReturn ~ lineFeed) | cariageReturn | lineFeed
  lazy val lineFeed = 0xA.p
  lazy val cariageReturn = 0xD.p

  lazy val white = space | tab
  lazy val space = 0x20.p
  lazy val tab = 0x9.p

  lazy val nonWhite = not(white) >> nonBreak
  lazy val nonBreak = not(break) >> allowedChar

  lazy val allowedChar = restrictChars(excludedChars)

  lazy val excludedChars = `C0 control block` ++ `C1 control block` ++ `surrogate block`
  lazy val `C0 control block` = (0x0 -> 0x1F) - 0x9 - 0xA - 0xD + 0x7F
  lazy val `C1 control block` = (0x80 -> 0x97) - 0x85
  lazy val `surrogate block` = (0xD800 -> 0xDFFF) + 0xFFFE + 0xFFFF

  lazy val uriChar =
    (uriEscapedChar | wordChar | '#' |
      ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' |
      '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' | '[' | ']') ^^ (_.toString)

  lazy val uriEscapedChar = '%' ~> hexDigitChar ~ hexDigitChar ^^ {
    case d1 ~ d2 => "%" + d1.toString + d2.toString
  }
  lazy val hexDigitChar = decDigitChar | (0x41 -> 0x46).p | (0x61 -> 0x66).p
  lazy val wordChar = decDigitChar | asciiLetterChar | '-'
  lazy val decDigitChar = (0x30 -> 0x39).p
  lazy val asciiLetterChar = (0x41 -> 0x5A).p | (0x61 -> 0x7A).p

  lazy val tagChar = not(flowIndicator) >> uriChar
  lazy val flowIndicator = success("") ~> ',' | '[' | ']' | '{' | '}'

  /* UTILS */

  implicit def stringToParser(s: String) = s.p

  implicit def c1[T](constructor: (String, Seq[String]) => T): Seq[Char] ~ Seq[Seq[Char]] => T = {
    case a ~ b => constructor(a, b)
  }

  implicit def c2[T](constructor: String => T): String ~ Seq[String] => T = {
    case a ~ b => constructor(a + b.mkString)
  }

  implicit def c3[A, B, T](constructor: (A, B) => T): A ~ B => T = {
    case a ~ b => constructor(a, b)
  }

  implicit def c4[T](constructor: String => T): List[Char] ~ List[Char] => T = {
    case a ~ b => constructor(a.mkString + "." + b.mkString)
  }

  private def removeNonDocuments(documents: Seq[Document]) =
    documents filterNot (_ == null)

  private implicit class StringEnhancements(string: String) {
    def p: Parser[String] =
      new Parser[String] {
        def apply(in: Input) = {
          val stringLength = string.length
          val source = in.source
          val sourceLength = source.length
          val offset = in.offset

          var posInString = 0
          var posInSource = offset
          while (posInString < stringLength &&
            posInSource < sourceLength &&
            string.charAt(posInString) == source.charAt(posInSource)) {
            posInString += 1
            posInSource += 1
          }

          val foundString = source.subSequence(offset, posInSource).toString

          if (posInString == stringLength)
            Success(foundString, in.drop(posInString))
          else {
            val found =
              if (offset == sourceLength) "end of source"
              else "'" + foundString + "'"
            Failure("'" + string + "' expected but " + found + " found", in.drop(posInString))
          }
        }
      }
  }

  private implicit class IntEnhancements(i: Int) {
    def ->(j: Int) = Seq.range(i, j).toSet
    def p: Parser[Char] = i.toChar
  }

  private implicit class SetEnhancements(s: Set[Int]) {
    def p: Parser[Char] = s.map(_.toChar).map(accept).reduce(_ | _)
  }

  /*
    Performance improvement

  private class CharParser(parser: Parser[Char]) {
    def + : Parser[String] = {
      new Parser[String] {
        def apply(in: Input) = {
          val s = new StringBuffer

          def parse(in: Input): ParseResult[Char] = {
            parser(in) match {
              case Success(elem, rest) =>
                s.append(elem)
                parse(rest)
              case ns: NoSuccess => ns
            }
          }

          parse(in) match {
            case Failure(_, rest) if (s.length > 0) => Success(s.toString, rest)
            case f @ Failure(_, _) => f
            case e @ Error(_, _) => e
            case unexpected => sys.error("Unexpected state: " + unexpected)
          }
        }
      }
    }

    def <~[T](p: Parser[T]) =
      new CharParser(parser <~ p)

  }
  */
  private def restrictChars(set: Set[Int]) = {
    val chars = set.map(_.toChar)
    new Parser[Char] {
      def apply(in: Input) = {
        val first = in.first
        if (in.atEnd) Failure("End of file encountered", in)
        else if (chars contains in.first) Error("Character not allowed: 0x" + first.toInt.toHexString, in)
        else Success(first, in.rest)
      }
    }
  }

  private def whenInput(check: Input => Boolean, message: String): Parser[_] =
    new Parser[Unit] {
      def apply(in: Input) =
        if (check(in)) Success((), in)
        else Failure(message, in)
    }

  implicit def toUnitFunction[T](p: Parser[T]): Unit => Parser[T] =
    (_) => p

  implicit def charSeqToNode(list: Seq[Any]): Node = Scalar(list.mkString)
  implicit def charSeqToString(list: Seq[Any]): String = list.mkString

  implicit def seqContent[A](list: Seq[A])(implicit ev: A => String): Seq[String] =
    list.map(ev)

  implicit def toConstructor1[A, B, C](constructor: A => B)(implicit ev: C => A): C => B =
    c => constructor(c)

  private class SkippingReader(in: Reader[Char], skip: Parser[_]) extends Reader[Char] {
    lazy val current = {
      skip(in) match {
        case Success(_, next) if (next.pos == in.pos) => in
        case Success(_, next: SkippingReader) => next
        case Success(_, next) => new SkippingReader(next, skip)
        case _ => in
      }
    }

    override lazy val source = current.source
    override lazy val offset = current.offset

    lazy val pos = current.pos
    lazy val atEnd = current.atEnd
    lazy val first = current.first
    lazy val rest = new SkippingReader(current.rest, skip)
  }
}