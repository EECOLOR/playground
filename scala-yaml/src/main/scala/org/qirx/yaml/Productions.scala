package org.qirx.yaml

import java.util.regex.Pattern

trait ProductionUtils {

  case class Range(chars: Seq[Either[Int, (Int, Int)]]) {
    def |(o: Range) = Range(chars ++ o.chars)
    def -(o: Range) = {
      val toRemove = o.chars.collect {
        case Left(char) => char
      }
      val newChars =
        chars.filterNot {
          case Left(char) => toRemove contains char
          case _ => false
        }

      Range(newChars)
    }
  }

  implicit def intToRange(i: Int) = Range(Seq(Left(i)))
  implicit def rangeToProduction(r: Range) = {
    val chars =
      r.chars.foldLeft("") { (chars, elem) =>

        def regExChar(char: Int) = "\\x{" + char.toHexString + "}"

        chars + elem.fold(regExChar, {
          case (char1, char2) => regExChar(char1) + "-" + regExChar(char2)
        })
      }
    Production("[" + chars + "]")
  }

  implicit def stringtoProduction(s: String): Production =
    Production(s)

  implicit class StringOps(s: String) {
    val p = Production(s)
  }

  implicit class IntOps(i: Int) {
    val c = Range(Seq(Left(i)))
    def ->(o: Int) = Range(Seq(Right((i, o))))
  }

  case class Production(s: String) {
    def |(o: Production) = copy(s + "|" + o.s)
    def ~(o: Production) = copy(s + o.s)
    def + = copy(s = s + "+")
    def ? = copy(s = s + "?")
    def ≡ = copy(s = "(" + s + ")")
    def ø = copy(s = "(?:" + s + ")")
    def * = copy(s = s + "*")

    def regex = s.r
  }
}

object Productions extends ProductionUtils {

  def augmentString(s: String) = ???
  def wrapString(s: String) = ???

  val eof = "\\z".p
  val lineStart = "^".p
  val lineEnd = "$".p
  val sTab = 0x9.c
  val bLineFeed = 0xA.c
  val bCarriageReturn = 0xD.c
  val sSpace = 0x20.c
  val cPrintable = (
    sTab | bLineFeed | bCarriageReturn | sSpace | 0x21 -> 0x7E |
    0x85.c | 0x40 -> 0xD7FF | 0xE00 -> 0xFFD |
    0x10000 -> 0x10FFFF)

  val sWhite = sSpace | sTab
  val sSeparateInLine = (sWhite.+ | lineStart).ø
  val bChar = bLineFeed | bCarriageReturn
  val nbChar = cPrintable - bChar

  val cNbCommentText = "#" ~ nbChar.*
  val bBreak = (bCarriageReturn ~ bLineFeed | bCarriageReturn | bLineFeed).ø
  val bNonContent = bBreak
  val bComment = (bNonContent | eof).ø
  val lComment = sSeparateInLine ~ cNbCommentText.≡.? ~ bComment

  val lDocumentPrefix = lComment.*
}