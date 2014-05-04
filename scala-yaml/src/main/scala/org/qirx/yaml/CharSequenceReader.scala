package org.qirx.yaml

import scala.util.parsing.input.{CharSequenceReader => ScalaCharSequenceReader}
import scala.util.parsing.input.Position

class CharSequenceReader(source: java.lang.CharSequence, offset:Int) extends ScalaCharSequenceReader(source, offset) {

  def this(source:java.lang.CharSequence) = this(source, 0)

  override def pos: Position = new OffsetPosition(source, offset)

  override def rest: CharSequenceReader =
    if (offset < source.length) new CharSequenceReader(source, offset + 1)
    else this
}