package org.qirx.yaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Reader
import scala.language.implicitConversions

object RawYamlParser extends Parsers {

  type Elem = Char

  def parse(s: String) =
    try {
      yamlStream(new SkippingReader(new CharSequenceReader(s), anyComment))
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }

  override def log[T](p: => Parser[T])(name: String): Parser[T] = Parser { in =>
    println(in.getClass.getName)
    println("trying " + name + " at " + in)
    val r = p(in)
    r match {
      case r @ Success(_, _) =>
        println(name + " --> " + r.toString.replaceAll("\r", "\\\\r").replaceAll("\n", "\\\\n"))
      case r =>
        println(name + " --> " + r)
    }
    r
  }

  lazy val anyComment = commentLine | comment
  lazy val commentLine = startOfLine ~ white.* ~ commentText.? ~ (break | endOfFile)
  lazy val comment = white.+ ~ commentText
  lazy val commentText = '#' ~ nonBreak.*

  lazy val startOfLine = whenInput({ x =>
    x.pos match {
      case pos: OffsetPosition => pos.column == 1
    }
  }, "Expected start of line")
  lazy val endOfFile = whenInput(_.atEnd, "Expected end of file")

  lazy val break = (cariageReturn ~ lineFeed) | cariageReturn | lineFeed
  lazy val lineFeed = 0xA.p
  lazy val cariageReturn = 0xD.p

  def marker(marker: String) =
    break.? ~ startOfLine ~ marker.p ~ (break | white | endOfFile)
  private lazy val directivesEnd = marker("---")
  private lazy val documentEnd = marker("...")

  //val breakChar = lineFeed | cariageReturn

  private val space = 0x20.p
  private val tab = 0x9.p
  private val white = space | tab

  private lazy val `C0 control block` = (0x0 -> 0x1F) - 0x9 - 0xA - 0xD + 0x7F
  private lazy val `C1 control block` = (0x80 -> 0x97) - 0x85
  private lazy val `surrogate block` = (0xD800 -> 0xDFFF) + 0xFFFE + 0xFFFF
  private lazy val excludedChars = `C0 control block` ++ `C1 control block` ++ `surrogate block`

  val anyChar = acceptIf(_ => true)(_ => "should not happen, this eats everything")

  private lazy val allowedChar =
    not(endOfFile) >> restrictChars(excludedChars)

  lazy val nonBreak =
    not(break) >> allowedChar

  lazy val forbidden =
    documentEnd | directivesEnd

  lazy val documentContent =
    not(forbidden) >> allowedChar

  case class Document(content: Option[String]) {
    def this(content: String) = this(Some(content))
  }
  object Document extends (String => Document) {
    def apply(content: String) = this(Some(content))
  }
  lazy val bareDocument =
    documentContent.+ <~ documentEnd.? ^^ Document

  lazy val explicitDocument =
    directivesEnd ~> bareDocument.? ^^ {
      case Some(document) => document
      case None => Document(None)
    }

  lazy val anyDocument =
    /*directive_document | */ explicitDocument | bareDocument

  lazy val noDocument =
    documentEnd ^^^ None

  private lazy val yamlStream =
    (noDocument | anyDocument).* ^^ (_.filter(_ != None))

  /*
  lazy val forbiddenDocumentContent =
    startOfLine & (directivesEnd | documentEnd) & (breakChar | white | endOfFile)
*/
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
    def p: Parser[Elem] = i.toChar
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

  implicit def charListToStringConstructor[T](constructor: String => T): List[Char] => T =
    list => constructor(list.mkString)

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