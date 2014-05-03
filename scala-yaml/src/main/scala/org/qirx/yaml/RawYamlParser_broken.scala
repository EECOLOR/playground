package org.qirx.yaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

object RawYamlParser_broken extends Parsers {

  // lazy val to lazy val
  // n:Int to n: => Int

  type Elem = Char

  lazy val na: Int = sys.error("Accessing non-applicable indentation")

  implicit def charToString(c: Char): String = c.toString

  implicit class IntEnhancements(i: Int) {
    private lazy val char = i.toChar
    def p: Parser[String] = accept(char) ^^^ (char.toString)

    private def betweenCharAnd(otherChar: Char) = { p: Char =>
      p >= char & p <= otherChar
    }

    def ->(other: Int): Parser[String] = {
      lazy val otherChar = other.toChar
      acceptIf(betweenCharAnd(otherChar))(
        "expected value between '$char' and '$otherChar', got '" + (_: Char) + "'") ^^ (_.toString)
    }

    def c = char
  }

  implicit class CharEnhancements(c: Char) {
    def p: Parser[String] = elem(c) ^^^ (c.toString)
  }

  implicit class ParserEnhancements[A](parser: Parser[A]) {
    def -[B <% A](other: Parser[B]): Parser[A] =
      not(other) ~> parser

    def *(times: Int): Parser[List[A]] =
      repN(times, parser)

    def as[B](implicit ev: A => B): Parser[B] =
      parser.map(ev)
  }

  sealed trait Empty
  object Empty extends Parser[Empty] with Empty {
    def apply(in: Input) = Success(this, in)
  }
  object StartOfLine extends Parser[String] {
    def apply(in: Input) =
      if (in.pos.column == 1) Success("", in)
      else Failure("Expected start of line", in)
  }
  lazy val EndOfFile = CharSequenceReader.EofCh

  sealed trait Context
  case object BlockIn extends Context
  case object BlockOut extends Context
  case object FlowOut extends Context
  case object FlowIn extends Context
  case object BlockKey extends Context
  case object FlowKey extends Context

  sealed trait Chomping
  case object Strip extends Chomping
  case object Clip extends Chomping
  case object Keep extends Chomping

  object AutoDetectIndentation extends Parser[Int] {
    def apply(in: Input) = {
      lazy val detector =
        (s_space.+ ~ b_non_content).* ~> (s_space.* <~ nb_char)

      detector(in) match {
        case Success(spaces, _) => Success(spaces.size, in)
        case _ => Failure("Could not autodetect indentation", in)
      }
    }
  }

  implicit class ParserFunctionEnhancement[T](p: (=> Int) => Parser[T]) {
    def <(max: Int) = {
      assert(max > 0)
      <=(max - 1)
    }
    def <=(max: Int) = {
      lazy val parsers = for (n <- max to 0 by -1) yield p(n)
      parsers.reduceLeft((result, p) => result | p)
    }
  }

  def unit(p: Parser[_]) = p ^^^ ()

  implicit def stringListToStringParser[A](c: Parser[List[A]])(implicit ev: A => String): Parser[String] =
    c ^^ (_.map(ev).mkString)

  implicit def toStringParser[A](c: Parser[A])(implicit ev: A => String): Parser[String] =
    c ^^ ev

  implicit def combine[A <% String, B <% String](c: A ~ B): String = {
    val ~(a, b) = c
    a + b
  }

  implicit def stringListToString[A](l: List[A])(implicit ev: A => String): String = l.map(ev).mkString

  implicit def optionToString[A](o: Option[A])(implicit ev: A => String): String = o.map(ev).getOrElse("")

  // 5.1. Character Set
  lazy val c_printable =
    0x09.p | 0xA.p | 0xD.p | (0x20 -> 0x7E) | /* 8 bit */
      0x85.p | (0xA0 -> 0xD7FF) | (0xE000 -> 0xFFD) | /* 16 bit */
      (0x10000 -> 0x10FFFF) /* 32 bit */

  lazy val nb_json = 0x9.p | (0x20 -> 0x10FFFF)

  // 5.3. Indicator Characters
  lazy val c_indicator = '-'.p | '?'.p | ':'.p | ','.p | '['.p | ']'.p | '{'.p | '}'.p |
    '#'.p | '&'.p | '*'.p | '!'.p | '|'.p | '>'.p | '\''.p | '"'.p |
    '%'.p | '@'.p | '`'.p

  lazy val c_flow_indicator = ','.p | '['.p | ']'.p | '{'.p | '}'.p

  // 5.4. Line Break Characters
  lazy val b_line_feed = 0xA.p /* LF */
  lazy val b_carriage_return = 0xD.p /* CR */
  lazy val b_char = b_line_feed | b_carriage_return

  lazy val nb_char = c_printable - b_char

  lazy val b_break = (
    (b_carriage_return ~ b_line_feed) /* DOS, Windows */
    | b_carriage_return /* MacOS upto 9.x */
    | b_line_feed /* UNIX, MacOS X */ ) ^^ {

      case (c1: String) ~ (c2: String) => c1 + c2
      case c: String => c.toString
    }

  lazy val b_as_line_feed = b_break

  lazy val b_non_content = b_break

  // 5.5. White Space Characters
  lazy val s_space = 0x20.p /* SP */
  lazy val s_tab = 0x09.p /* TAB */
  lazy val s_white = s_space | s_tab

  lazy val ns_char = nb_char - s_white

  // 5.6. Miscellaneous Characters
  lazy val ns_dec_digit = (0x30 -> 0x39) /* 0-9 */

  lazy val ns_hex_digit =
    ns_dec_digit |
      (0x41 -> 0x46) /* A-F */ | (0x61 -> 0x66) /* a-f */

  lazy val ns_ascii_letter = (0x41 -> 0x5A) /* A-Z */ | (0x61 -> 0x7A) /* a-z */

  lazy val ns_word_char = ns_dec_digit | ns_ascii_letter | '-'.p

  lazy val ns_uri_char = (
    ('%' ~> ns_hex_digit ~ ns_hex_digit) | ns_word_char | '#'.p
    | ';'.p | '/'.p | '?'.p | ':'.p | '@'.p | '&'.p | '='.p | '+'.p | '$'.p | ','.p
    | '_'.p | '.'.p | '!'.p | '~'.p | '*'.p | '\''.p | '('.p | ')'.p | '['.p | ']'.p) ^^ {

      case (c1: String) ~ (c2: String) => "%" + c1 + c2
      case c: String => c.toString
    }

  lazy val ns_tag_char = ns_uri_char - '!'.p - c_flow_indicator

  // 5.7. Escaped Characters

  lazy val ns_esc_null = '0'.p
  lazy val ns_esc_bell = 'a'.p
  lazy val ns_esc_backspace = 'b'.p
  lazy val ns_esc_horizontal_tab = 't'.p | 0x9.p
  lazy val ns_esc_line_feed = 'n'.p
  lazy val ns_esc_vertical_tab = 'v'.p
  lazy val ns_esc_form_feed = 'f'.p
  lazy val ns_esc_carriage_return = 'r'.p
  lazy val ns_esc_escape = 'e'.p
  lazy val ns_esc_space = 0x20.p
  lazy val ns_esc_double_quote = '"'.p
  lazy val ns_esc_slash = '/'.p
  lazy val ns_esc_backslash = '\\'.p
  lazy val ns_esc_next_line = 'N'.p
  lazy val ns_esc_non_breaking_space = '_'.p
  lazy val ns_esc_line_separator = 'L'.p
  lazy val ns_esc_paragraph_separator = 'P'.p
  lazy val ns_esc_8_bit: Parser[String] = 'x' ~ (ns_hex_digit * 2)
  lazy val ns_esc_16_bit: Parser[String] = 'u' ~ (ns_hex_digit * 4)
  lazy val ns_esc_32_bit: Parser[String] = 'U' ~ (ns_hex_digit * 8)

  lazy val c_ns_esc_char = ('\\' ~>
    (ns_esc_null | ns_esc_bell | ns_esc_backspace
      | ns_esc_horizontal_tab | ns_esc_line_feed
      | ns_esc_vertical_tab | ns_esc_form_feed
      | ns_esc_carriage_return | ns_esc_escape | ns_esc_space
      | ns_esc_double_quote | ns_esc_slash | ns_esc_backslash
      | ns_esc_next_line | ns_esc_non_breaking_space
      | ns_esc_line_separator | ns_esc_paragraph_separator
      | ns_esc_8_bit | ns_esc_16_bit | ns_esc_32_bit)) ^^ {
        "\\" + _.toString
      }

  // Chapter 6. Basic Structures
  // 6.1. Indentation Spaces
  lazy val s_indent = {
    def s_indent(n: => Int) = unit(s_space * (if (n > 0) n else 0))
    s_indent _
  }

  // 6.2. Separation Spaces
  lazy val s_separate_in_line = unit(s_white.+ | StartOfLine)

  // 6.3. Line Prefixes
  def s_line_prefix(n: => Int, c: Context) =
    c match {
      case BlockOut | BlockIn => s_block_line_prefix(n)
      case FlowOut | FlowIn => s_flow_line_prefix(n)
      case unexpected => sys.error("Unexpected context: " + unexpected)
    }
  def s_block_line_prefix(n: => Int) = s_indent(n)
  def s_flow_line_prefix(n: => Int) = unit(s_indent(n) ~ s_separate_in_line.?)

  // 6.4. Empty Lines
  def l_empty(n: => Int, c: Context) =
    unit((s_line_prefix(n, c) | (s_indent < n)) ~ b_as_line_feed)

  // 6.5. Line Folding
  def b_l_trimmed(n: => Int, c: Context) = unit(b_non_content ~ l_empty(n, c).+)
  lazy val b_as_space = unit(b_break)
  def b_l_folded(n: => Int, c: Context) =
    b_l_trimmed(n, c) | b_as_space

  def s_flow_folded(n: => Int) = unit(
    s_separate_in_line.? ~ b_l_folded(n, FlowIn) ~
      s_flow_line_prefix(n))

  // 6.6. Comments
  case class Comment(text: String)
  trait Comments[T] {
    val comments: List[Comment]
    def withComments(c: List[Comment]): T
  }
  lazy val c_nb_comment_text = ('#' ~> nb_char.*) ^^ (Comment apply _)
  lazy val b_comment = unit(b_non_content | EndOfFile.p)
  lazy val s_b_comment = ((s_separate_in_line ~> c_nb_comment_text.?).? ^^ (_.flatten)) <~ b_comment
  lazy val l_comment = s_separate_in_line ~> c_nb_comment_text.? <~ b_comment
  lazy val s_l_comments = (s_b_comment | StartOfLine) ~> l_comment.* ^^ (_.flatten)

  // 6.7. Separation Lines
  def s_separate(n: => Int, c: Context) =
    c match {
      case BlockOut | BlockIn | FlowOut | FlowIn => s_separate_lines(n)
      case BlockKey | FlowKey => s_separate_in_line ^^^ List.empty
    }
  def s_separate_lines(n: => Int) = (s_l_comments <~ s_flow_line_prefix(n)) |
    s_separate_in_line ^^^ List.empty

  // 6.8. Directives
  sealed trait Directive extends Comments[Directive]
  lazy val l_directive = ('%' ~>
    (ns_yaml_directive | ns_tag_directive | ns_reserved_directive) ~
    s_l_comments) ^^ {
      case directive ~ comments => directive.withComments(comments)
    }
  case class ReservedDirective(name: String, parameters: List[String])(val comments: List[Comment] = List.empty) extends Directive {
    def withComments(c: List[Comment]): Directive = this.copy()(c ::: comments)
  }
  lazy val ns_reserved_directive =
    (ns_directive_name ~ (s_separate_in_line ~> ns_directive_parameter).*) ^^ {
      case name ~ params => ReservedDirective(name, params)()
    }
  lazy val ns_directive_name = ns_char.+
  lazy val ns_directive_parameter: Parser[String] = ns_char.+
  // 6.8.1. “YAML” Directives
  case class YamlDirective(version: String)(val comments: List[Comment] = List.empty) extends Directive {
    def withComments(c: List[Comment]): Directive = this.copy()(c ::: comments)
  }
  lazy val ns_yaml_directive =
    'Y' ~ 'A' ~ 'M' ~ 'L' ~ s_separate_in_line ~> ns_yaml_version
  lazy val ns_yaml_version =
    (ns_dec_digit.+ ~ ('.'.p ~> ns_dec_digit.+)) ^^ {
      case d1 ~ d2 => YamlDirective(d1 + "." + d2)()
    }
  // 6.8.2. “TAG” Directives
  case class TagDirective(handle: TagHandle, prefix: String)(val comments: List[Comment] = List.empty) extends Directive {
    def withComments(c: List[Comment]): Directive = this.copy()(c ::: comments)
  }
  lazy val ns_tag_directive = 'T' ~ 'A' ~ 'G' ~
    s_separate_in_line ~> c_tag_handle ~ (s_separate_in_line ~> ns_tag_prefix) ^^ {
      case handle ~ prefix => TagDirective(handle, prefix)()
    }
  // 6.8.2.1. Tag Handles
  trait TagHandle
  case object PrimaryTagHandle extends TagHandle
  case object SecondaryTagHandle extends TagHandle
  case class NamedTagHandle(name: String) extends TagHandle
  lazy val c_tag_handle = c_named_tag_handle | c_secondary_tag_handle | c_primary_tag_handle
  lazy val c_primary_tag_handle = '!' ^^^ PrimaryTagHandle
  lazy val c_secondary_tag_handle = '!' ~ '!' ^^^ SecondaryTagHandle
  lazy val c_named_tag_handle = '!' ~> ns_word_char.+ <~ '!' ^^ (NamedTagHandle apply _)
  // 6.8.2.2. Tag Prefixes
  lazy val ns_tag_prefix = c_ns_local_tag_prefix | ns_global_tag_prefix
  lazy val c_ns_local_tag_prefix: Parser[String] = '!' ~> ns_uri_char.*
  lazy val ns_global_tag_prefix: Parser[String] = (ns_tag_char ~ ns_uri_char.*)

  // 6.9. Node Properties
  def c_ns_properties(n: => Int, c: Context): Parser[(Option[NodeTag], Option[Anchor])] =
    ((c_ns_tag_property ~
      (s_separate(n, c) ~> c_ns_anchor_property).?) |
      (c_ns_anchor_property ~
        (s_separate(n, c) ~> c_ns_tag_property).?)) ^^ {
          case (tag: NodeTag) ~ (anchor: Option[Anchor]) =>
            Some(tag) -> anchor
          case (anchor: Anchor) ~ (tag: Option[NodeTag]) =>
            tag -> Some(anchor)
        }

  // 6.9.1. Node Tags
  sealed trait NodeTag
  lazy val c_ns_tag_property =
    c_verbatim_tag |
      c_ns_shorthand_tag |
      c_non_specific_tag

  case class VerbatimTag(name: String) extends NodeTag
  lazy val c_verbatim_tag = ('!'.p ~ '<'.p ~> ns_uri_char.+ <~ '>') ^^ (VerbatimTag apply _)

  case class ShorthandTag(handle: TagHandle, name: String) extends NodeTag
  lazy val c_ns_shorthand_tag = (c_tag_handle ~ ns_tag_char.+) ^^ {
    case handle ~ name => ShorthandTag(handle, name)
  }

  case object NonSpecificTag extends NodeTag
  lazy val c_non_specific_tag = '!' ^^^ NonSpecificTag

  // 6.9.2. Node Anchors
  case class Anchor(name: String)
  lazy val c_ns_anchor_property = '&' ~> ns_anchor_name ^^ (Anchor apply _)

  lazy val ns_anchor_char = ns_char - c_flow_indicator
  lazy val ns_anchor_name = ns_anchor_char.+

  // Chapter 7. Flow Styles
  sealed trait Node
  // 7.1. Alias Nodes
  case class AliasNode(alias: String) extends Node
  lazy val c_ns_alias_node = '*' ~> ns_anchor_name ^^ (AliasNode apply _)

  // 7.2. Empty Nodes
  case object EmptyScalar extends Node
  lazy val e_scalar = Empty ^^^ EmptyScalar
  case object EmptyNode extends Node
  lazy val e_node = e_scalar ^^^ EmptyNode

  // 7.3. Flow Scalar Styles
  // 7.3.1. Double-Quoted Style
  lazy val nb_double_char = c_ns_esc_char | (nb_json - '\\'.p - '"'.p)
  lazy val ns_double_char = nb_double_char - s_white

  case class SimpleNode(value: String) extends Node
  def c_double_quoted(n: => Int, c: Context): Parser[SimpleNode] =
    ('"' ~> nb_double_text(n, c) <~ '"') ^^ SimpleNode
  def nb_double_text(n: => Int, c: Context): Parser[String] =
    c match {
      case FlowOut | FlowIn => nb_double_multi_line(n)
      case BlockKey | FlowKey => nb_double_one_line
      case unexpected => sys.error("Unexpected context: " + unexpected)
    }
  lazy val nb_double_one_line = nb_double_char.*

  def s_double_escaped(n: => Int) =
    s_white.* ~ '\\' ~ b_non_content ~
      l_empty(n, FlowIn).* ~ s_flow_line_prefix(n)
  def s_double_break(n: => Int) = s_double_escaped(n) | s_flow_folded(n)

  lazy val nb_ns_double_in_line: Parser[String] = (s_white.* ~ ns_double_char).*
  def s_double_next_line(n: => Int): Parser[String] =
    s_double_break(n) ~>
      (ns_double_char ~ nb_ns_double_in_line ~
        (s_double_next_line(n) | s_white.*.as[String])).?
  def nb_double_multi_line(n: => Int) =
    nb_ns_double_in_line ~
      (s_double_next_line(n) | s_white.*.as[String])

  // 7.3.2. Single-Quoted Style
  lazy val c_quoted_quote: Parser[String] = '\'' ~ '\''
  lazy val nb_single_char = c_quoted_quote | (nb_json - '\'')
  lazy val ns_single_char = nb_single_char - s_white

  def c_single_quoted(n: => Int, c: Context): Parser[SimpleNode] =
    ('\'' ~> nb_single_text(n, c) <~ '\'') ^^ SimpleNode
  def nb_single_text(n: => Int, c: Context): Parser[String] =
    c match {
      case FlowOut | FlowIn => nb_single_multi_line(n)
      case BlockKey | FlowKey => nb_single_one_line
      case unexpected => sys.error("Unexpected context: " + unexpected)
    }
  lazy val nb_single_one_line = nb_single_char.*

  lazy val nb_ns_single_in_line = (s_white.* ~ ns_single_char).*
  def s_single_next_line(n: => Int): Parser[String] =
    s_flow_folded(n) ~>
      (ns_single_char ~ nb_ns_single_in_line ~
        (s_single_next_line(n) | s_white.*.as[String])).?
  def nb_single_multi_line(n: => Int) =
    nb_ns_single_in_line ~
      (s_single_next_line(n) | s_white.*.as[String])

  // 7.3.3. Plain Style
  def ns_plain_first(c: Context) = (ns_char - c_indicator) |
    (('?'.p | ':'.p | '-'.p) ~ guard(ns_plain_safe(c))).as[String]

  def ns_plain_safe(c: Context) =
    c match {
      case FlowOut | BlockKey => ns_plain_safe_out
      case FlowIn | FlowKey => ns_plain_safe_in
      case unexpected => sys.error("Unexpected context: " + unexpected)
    }
  def ns_plain_safe_out = ns_char
  def ns_plain_safe_in = ns_char - c_flow_indicator
  def ns_plain_char(c: Context) =
    (ns_plain_safe(c) - ':'.p - '#'.p) |
      ((ns_char ~ guard('#'.p)) |
        (':'.p ~ guard(ns_plain_safe(c)))).as[String]

  def ns_plain(n: => Int, c: Context): Parser[String] =
    c match {
      case FlowOut | FlowIn => ns_plain_multi_line(n, c)
      case BlockKey | FlowKey => ns_plain_one_line(c)
      case unexpected => sys.error("Unexpected context: " + unexpected)
    }
  def nb_ns_plain_in_line(c: Context): Parser[String] = (s_white.* ~ ns_plain_char(c)).*
  def ns_plain_one_line(c: Context) = ns_plain_first(c) ~ nb_ns_plain_in_line(c)

  def s_ns_plain_next_line(n: => Int, c: Context) =
    s_flow_folded(n) ~>
      ns_plain_char(c) ~ nb_ns_plain_in_line(c)
  def ns_plain_multi_line(n: => Int, c: Context) =
    ns_plain_one_line(c) ~
      s_ns_plain_next_line(n, c).*

  // 7.4. Flow Collection Styles
  def in_flow(c: Context) =
    c match {
      case FlowOut => FlowIn
      case FlowIn => FlowIn
      case BlockKey => FlowKey
      case FlowKey => FlowKey
      case unexpected => sys.error("Unexpected context: " + unexpected)
    }

  // 7.4.1. Flow Sequences
  case class SeqNode(seq: Seq[Node]) extends Node
  def c_flow_sequence(n: => Int, c: Context): Parser[SeqNode] =
    ('[' ~ s_separate(n, c).? ~>
      ns_s_flow_seq_entries(n, in_flow(c)).? <~ ']') ^^ (SeqNode apply _.getOrElse(Seq.empty))

  def ns_s_flow_seq_entries(n: => Int, c: Context): Parser[Seq[Node]] =
    ((ns_flow_seq_entry(n, c) <~ s_separate(n, c).?) ~
      (',' ~ s_separate(n, c).? ~>
        ns_s_flow_seq_entries(n, c).?).?) ^^ {
          case node ~ nodes => node +: nodes.getOrElse(None).getOrElse(Seq.empty)
        }

  def ns_flow_seq_entry(n: => Int, c: Context): Parser[Node] =
    ns_flow_pair(n, c) | ns_flow_node(n, c)

  // 7.4.2. Flow Mappings
  case class MappingNode(entries: Seq[(Node, Node)]) extends Node
  def c_flow_mapping(n: => Int, c: Context): Parser[MappingNode] =
    ('{' ~ s_separate(n, c).? ~>
      ns_s_flow_map_entries(n, in_flow(c)).? <~ '}') ^^ {
        MappingNode apply _.getOrElse(Seq.empty)
      }

  def ns_s_flow_map_entries(n: => Int, c: Context): Parser[Seq[(Node, Node)]] =
    ((ns_flow_map_entry(n, c) <~ s_separate(n, c).?) ~
      (',' ~ s_separate(n, c).? ~>
        ns_s_flow_map_entries(n, c).?).?) ^^ {
          case pair ~ pairs => pair +: pairs.getOrElse(None).getOrElse(Seq.empty)
        }

  def ns_flow_map_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    ('?' ~ s_separate(n, c) ~>
      ns_flow_map_explicit_entry(n, c)) |
      ns_flow_map_implicit_entry(n, c)
  def ns_flow_map_explicit_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    ns_flow_map_implicit_entry(n, c) |
      (e_node /* Key */ ~ e_node /* Value */ ) ^^ {
        case key ~ value => key -> value
      }

  def ns_flow_map_implicit_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    ns_flow_map_yaml_key_entry(n, c) |
      c_ns_flow_map_empty_key_entry(n, c) |
      c_ns_flow_map_json_key_entry(n, c)
  def ns_flow_map_yaml_key_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    (ns_flow_yaml_node(n, c) ~
      ((s_separate(n, c).? ~>
        c_ns_flow_map_separate_value(n, c)) |
        e_node)) ^^ {
          case key ~ value => key -> value
        }
  def c_ns_flow_map_empty_key_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    (e_node /* Key */ ~
      c_ns_flow_map_separate_value(n, c)) ^^ {
        case key ~ value => key -> value
      }
  def c_ns_flow_map_separate_value(n: => Int, c: Context): Parser[Node] =
    log(
    ':' ~ not(ns_plain_safe(c)) ~>
      ((s_separate(n, c) ~> ns_flow_node(n, c)) |
        e_node /* Value */ ))("c_ns_flow_map_separate_value")

  def c_ns_flow_map_json_key_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    (c_flow_json_node(n, c) ~
      ((s_separate(n, c).? ~>
        c_ns_flow_map_adjacent_value(n, c)) |
        e_node)) ^^ {
          case key ~ value => key -> value
        }
  def c_ns_flow_map_adjacent_value(n: => Int, c: Context): Parser[Node] =
    ':' ~> ((s_separate(n, c).? ~>
      ns_flow_node(n, c)) |
      e_node) /* Value */

  case class Pair(values: (Node, Node)) extends Node
  def ns_flow_pair(n: => Int, c: Context): Parser[Node] =
    (('?' ~ s_separate(n, c) ~>
      ns_flow_map_explicit_entry(n, c)) |
      ns_flow_pair_entry(n, c)) ^^ Pair

  def ns_flow_pair_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    ns_flow_pair_yaml_key_entry(n, c) |
      c_ns_flow_map_empty_key_entry(n, c) |
      c_ns_flow_pair_json_key_entry(n, c)
  def ns_flow_pair_yaml_key_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    (ns_s_implicit_yaml_key(FlowKey) ~
      c_ns_flow_map_separate_value(n, c)) ^^ {
        case key ~ value => key -> value
      }
  def c_ns_flow_pair_json_key_entry(n: => Int, c: Context): Parser[(Node, Node)] =
    (c_s_implicit_json_key(FlowKey) ~
      c_ns_flow_map_adjacent_value(n, c)) ^^ {
        case key ~ value => key -> value
      }
  def ns_s_implicit_yaml_key(c: Context) =
    log(ns_flow_yaml_node(na, c) <~ s_separate_in_line.?)("ns_s_implicit_yaml_key")
  /* At most 1024 characters altogether */
  def c_s_implicit_json_key(c: Context) =
    log(c_flow_json_node(na, c) <~ s_separate_in_line.?)("c_s_implicit_json_key")
  /* At most 1024 characters altogether */

  // 7.5. Flow Nodes
  case class YamlContent(content: String) extends Node
  def ns_flow_yaml_content(n: => Int, c: Context) = ns_plain(n, c) ^^ YamlContent
  def c_flow_json_content(n: => Int, c: Context): Parser[Node] =
    log(
    c_flow_sequence(n, c) | c_flow_mapping(n, c) |
      c_single_quoted(n, c) | c_double_quoted(n, c))("c_flow_json_content")

  def ns_flow_content(n: => Int, c: Context) =
    ns_flow_yaml_content(n, c) | c_flow_json_content(n, c)

  case class NodeWithProperties(node: Node, tag: Option[NodeTag], anchor: Option[Anchor]) extends Node
  def ns_flow_yaml_node(n: => Int, c: Context): Parser[Node] =
    log(
    c_ns_alias_node |
      ns_flow_yaml_content(n, c) |
      (c_ns_properties(n, c) ~
        ((s_separate(n, c) ~>
          ns_flow_yaml_content(n, c)) |
          e_scalar)) ^^ {
            case (tag, anchor) ~ node => NodeWithProperties(node, tag, anchor)
          })("ns_flow_yaml_node")
  def c_flow_json_node(n: => Int, c: Context): Parser[Node] =
    log(
    ((c_ns_properties(n, c) <~ s_separate(n, c)).? ~
      c_flow_json_content(n, c)) ^^ {
        case properties ~ node =>
          properties.map {
            case (tag, anchor) => NodeWithProperties(node, tag, anchor)
          }.getOrElse(node)
      })("c_flow_json_node")
  def ns_flow_node(n: => Int, c: Context): Parser[Node] =
    c_ns_alias_node |
      ns_flow_content(n, c) |
      (c_ns_properties(n, c) ~
        ((s_separate(n, c) ~>
          ns_flow_content(n, c)) |
          e_scalar)) ^^ {
            case (tag, anchor) ~ node =>
              NodeWithProperties(node, tag, anchor)
          }

  // 8.1. Block Scalar Styles
  // 8.1.1. Block Scalar Headers
  def c_b_block_header = new {
    def ~>[U](innerParser: (Int, Chomping) => Parser[U]) = {

      lazy val c_b_block_header =
        ((c_indentation_indicator ~ c_chomping_indicator) ^^ { case m ~ t => (m, t) }
          | (c_chomping_indicator ~ c_indentation_indicator) ^^ { case t ~ m => (m, t) }) ~ s_b_comment

      c_b_block_header >> {
        case (m, t) ~ comment =>
          innerParser(m, t)
      }
    }
  }

  // 8.1.1.1. Block Indentation Indicator
  lazy val c_indentation_indicator = ((ns_dec_digit - 0x30.p) | Empty) >> {
    case Empty => AutoDetectIndentation
    case i: String => success(i.toInt)
  }

  // 8.1.1.2. Block Chomping Indicator
  lazy val c_chomping_indicator = ('-'.p | '+'.p | Empty) ^^ {
    case "-" => Strip
    case "+" => Keep
    case Empty => Clip
  }
  def b_chomped_last(t: Chomping) =
    t match {
      case Strip => b_non_content | EndOfFile
      case Clip => b_as_line_feed | EndOfFile
      case Keep => b_as_line_feed | EndOfFile
    }
  def l_chomped_empty(n: => Int, t: Chomping) =
    t match {
      case Strip => l_strip_empty(n)
      case Clip => l_strip_empty(n)
      case Keep => l_keep_empty(n)
    }
  def l_strip_empty(n: => Int) = ((s_indent <= n) ~ b_non_content).* ~
    l_trail_comments(n).?
  def l_keep_empty(n: => Int) = l_empty(n, BlockIn).* ~
    l_trail_comments(n).?
  def l_trail_comments(n: => Int) = (s_indent < n) ~ c_nb_comment_text ~ b_comment ~
    l_comment.*

  // 8.1.2. Literal Style
  def c_l_literal(n: => Int) =
    '|' ~> (c_b_block_header ~> { (m, t) =>
      l_literal_content(n + m, t)
    })

  def l_nb_literal_text(n: => Int) =
    l_empty(n, BlockIn).* ~
      s_indent(n) ~> nb_char.+
  def b_nb_literal_next(n: => Int) = b_as_line_feed ~ l_nb_literal_text(n)
  def l_literal_content(n: => Int, t: Chomping): Parser[Node] =
    ((l_nb_literal_text(n) ~ b_nb_literal_next(n).* <~ b_chomped_last(t)).? <~
      l_chomped_empty(n, t)) ^^ (SimpleNode apply _)

  // 8.1.3. Folded Style
  def c_l_folded(n: => Int): Parser[SimpleNode] =
    '>' ~>
      (c_b_block_header ~> { (m, t) =>
        l_folded_content(n + m, t) ^^ (SimpleNode apply _)
      })

  def s_nb_folded_text(n: => Int) =
    s_indent(n) ~> ns_char ~ nb_char.*
  def l_nb_folded_lines(n: => Int): Parser[String] =
    s_nb_folded_text(n) ~
      (b_l_folded(n, BlockIn) ~> s_nb_folded_text(n)).*

  def s_nb_spaced_text(n: => Int) =
    s_indent(n) ~> s_white ~ nb_char.*
  def b_l_spaced(n: => Int) =
    b_as_line_feed <~
      l_empty(n, BlockIn).*
  def l_nb_spaced_lines(n: => Int): Parser[String] =
    s_nb_spaced_text(n) ~>
      (b_l_spaced(n) ~ s_nb_spaced_text(n)).*

  def l_nb_same_lines(n: => Int) =
    l_empty(n, BlockIn).* ~>
      (l_nb_folded_lines(n) | l_nb_spaced_lines(n))
  def l_nb_diff_lines(n: => Int) =
    l_nb_same_lines(n) ~
      (b_as_line_feed ~> l_nb_same_lines(n)).*

  def l_folded_content(n: => Int, t: Chomping) =
    (l_nb_diff_lines(n) <~ b_chomped_last(t)).? <~
      l_chomped_empty(n, t)

  // 8.2.1. Block Sequences
  def l_block_sequence(n: => Int): Parser[SeqNode] =
    log(
      AutoDetectIndentation >> { m =>
        if (m > n)
          ((s_indent(n + m) ~> c_l_block_seq_entry(n + m)).+) ^^ SeqNode
        else
          failure("Expected an indentation of at least" + (n + 1))
      })("l_block_sequence")
  def c_l_block_seq_entry(n: => Int): Parser[Node] =
    '-' ~ not(ns_char) ~> s_l_block_indented(n, BlockIn)

  def s_l_block_indented(n: => Int, c: Context): Parser[Node] =
    AutoDetectIndentation >> { m =>
      if (m > n)
        (s_indent(m) ~>
          (ns_l_compact_sequence(n + 1 + m) |
            ns_l_compact_mapping(n + 1 + m))) |
            s_l_block_node(n, c) |
            (e_node <~ s_l_comments)
      else
        failure("Expected an indentation of at least" + (n + 1))

    }
  def ns_l_compact_sequence(n: => Int): Parser[SeqNode] =
    (c_l_block_seq_entry(n) ~
      (s_indent(n) ~> c_l_block_seq_entry(n)).*) ^^ {
        case entry ~ entries => SeqNode(entry +: entries)
      }

  // 8.2.2. Block Mappings
  def l_block_mapping(n: => Int): Parser[MappingNode] =
    log(
      AutoDetectIndentation >> { m =>
        if (m > n)
          ((s_indent(n + m) ~> ns_l_block_map_entry(n + m)).+) ^^ MappingNode
        else
          failure("Expected an indentation of at least" + (n + 1))
      })("l_block_mapping")

  def ns_l_block_map_entry(n: => Int): Parser[(Node, Node)] =
    c_l_block_map_explicit_entry(n) |
      ns_l_block_map_implicit_entry(n)
  def c_l_block_map_explicit_entry(n: => Int): Parser[(Node, Node)] =
    (c_l_block_map_explicit_key(n) ~
      (l_block_map_explicit_value(n) | e_node)) ^^ {
        case key ~ value => key -> value
      }
  def c_l_block_map_explicit_key(n: => Int): Parser[Node] =
    '?' ~> s_l_block_indented(n, BlockOut)
  def l_block_map_explicit_value(n: => Int): Parser[Node] =
    s_indent(n) ~
      ':' ~> s_l_block_indented(n, BlockOut)

  def ns_l_block_map_implicit_entry(n: => Int): Parser[(Node, Node)] =
    log(
    ((ns_s_block_map_implicit_key | e_node) ~
      c_l_block_map_implicit_value(n)) ^^ {
        case key ~ value => key -> value
      })("ns_l_block_map_implicit_entry")
  def ns_s_block_map_implicit_key =
    log(
    c_s_implicit_json_key(BlockKey) |
      ns_s_implicit_yaml_key(BlockKey))("ns_s_block_map_implicit_key")

  def c_l_block_map_implicit_value(n: => Int): Parser[Node] =
    ':' ~> (s_l_block_node(n, BlockOut) |
      (e_node <~ s_l_comments))

  def ns_l_compact_mapping(n: => Int): Parser[MappingNode] =
    (ns_l_block_map_entry(n) ~
      (s_indent(n) ~> ns_l_block_map_entry(n)).*) ^^ {
        case entry ~ entries => MappingNode(entry +: entries)
      }

  // 8.2.3. Block Nodes
  def s_l_block_node(n: => Int, c: Context): Parser[Node] =
    log(s_l_block_in_block(n, c) | s_l_flow_in_block(n))("s_l_block_node")
  def s_l_flow_in_block(n: => Int): Parser[Node] =
    s_separate(n + 1, FlowOut) ~>
      ns_flow_node(n + 1, FlowOut) <~ s_l_comments

  def s_l_block_in_block(n: => Int, c: Context): Parser[Node] =
    log(s_l_block_scalar(n, c) | s_l_block_collection(n, c))("s_l_block_in_block")
  def s_l_block_scalar(n: => Int, c: Context): Parser[Node] =
    log(
      s_separate(n + 1, c) ~>
        (c_ns_properties(n + 1, c) <~ s_separate(n + 1, c)).? ~
        (c_l_literal(n) | c_l_folded(n)) ^^ {
          case properties ~ node =>
            properties.map {
              case (tag, anchor) => NodeWithProperties(node, tag, anchor)
            }.getOrElse(node)
        })("s_l_block_scalar")

  def s_l_block_collection(n: => Int, c: Context): Parser[Node] =
    log(
      (((s_separate(n + 1, c) ~> c_ns_properties(n + 1, c)).? <~
        s_l_comments) ~
        (l_block_sequence(seq_spaces(n, c)) | l_block_mapping(n))) ^^ {
          case properties ~ node =>
            properties.map {
              case (tag, anchor) => NodeWithProperties(node, tag, anchor)
            }.getOrElse(node)
        })("s_l_block_collection")
  def seq_spaces(n: => Int, c: Context) =
    c match {
      case BlockOut => n - 1
      case BlockIn => n
      case unexpected => sys.error("Unexpected context in seq_spaces: " + unexpected)
    }

  // Chapter 9. YAML Character Stream

  // 9.1. Documents
  // 9.1.1. Document Prefix
  lazy val l_document_prefix = log(l_comment.+)("l_document_prefix")
  // 9.1.2. Document Markers
  lazy val c_directives_end: Parser[String] = log('-' ~ '-' ~ '-')("c_directives_end")
  lazy val c_document_end: Parser[String] = log('.' ~ '.' ~ '.')("c_document_end")
  lazy val l_document_suffix = log(c_document_end ~ s_l_comments)("l_document_suffix")
  // 9.1.3. Bare Documents
  case class Document(node: Node, directives: Seq[Directive] = Seq.empty)
  lazy val l_bare_document =
    log(
      s_l_block_node(-1, BlockIn) ^^ (Document apply _))("l_bare_document")
  // 9.1.4. Explicit Documents
  lazy val l_explicit_document =
    log(
      c_directives_end ~> (l_bare_document | (e_node <~ s_l_comments)) ^^ {
        case d: Document => d
        case node: Node => Document(node)
      })("l_explicit_document")
  // 9.1.5. Directives Documents
  lazy val l_directive_document =
    log(
      (l_directive.+ ~ l_explicit_document) ^^ {
        case directives ~ document => document.copy(directives = directives)
      })("l_directive_document")

  // 9.2. Streams
  lazy val l_any_document =
    log(
      l_directive_document | l_explicit_document | l_bare_document)("l_any_document")

  lazy val l_yaml_stream =
    (l_document_prefix.* ~> l_any_document.? ~
      (l_document_suffix.+ ~ l_document_prefix.* ~> l_any_document.?
        | l_document_prefix.* ~> l_explicit_document.?).+) ^^ {
          case document ~ documents =>
            (document +: documents).flatten
        }
}