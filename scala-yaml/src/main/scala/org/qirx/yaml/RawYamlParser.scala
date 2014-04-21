package org.qirx.yaml

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.CharSequenceReader

object RawYamlParser extends Parsers {

  // val to lazy val
  // n:Int to n: => Int

  type Elem = Char

  lazy val na: Int = sys.error("Accessing non-applicable indentation")

  implicit class IntEnhancements(i: Int) {
    private val char = i.toChar
    def p = accept(char)

    private def betweenCharAnd(otherChar: Char) = { p: Char =>
      p >= char & p <= otherChar
    }

    def ->(other: Int): Parser[Char] = {
      val otherChar = other.toChar
      acceptIf(betweenCharAnd(otherChar))(
        "expected value between '$char' and '$otherChar', got '" + (_: Char) + "'")
    }

    def c = char
  }

  implicit class CharEnhancements(c: Char) {
    def p = elem(c)
  }

  implicit class ParserEnhancements[T](parser: Parser[T]) {
    def -(other: Parser[T]): Parser[T] =
      not(other) ~> parser

    def *(times: Int): Parser[List[T]] =
      repN(times, parser)
  }

  sealed trait Empty
  object Empty extends Parser[Empty] with Empty {
    def apply(in: Input) = Success(this, in)
  }
  object StartOfLine extends Parser[Unit] {
    def apply(in: Input) =
      if (in.pos.column == 1) Success((), in)
      else Failure("Expected start of line", in)
  }
  val EndOfFile = CharSequenceReader.EofCh

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
      val detector =
        (s_space.+ ~ b_non_content).* ~> (s_space.+ <~ nb_char)

      detector(in) match {
        case Success(spaces, _) => Success(spaces.size, in)
        case _ => Failure("Could not autodetect indentation", in)
      }
    }
  }

  sealed trait Indentation
  object AutoDetect extends Indentation
  case class IndentationValue(i: Int) extends Indentation

  implicit class ParserFunctionEnhancement[T](p: Int => Parser[T]) {
    def <(max: Int) = {
      assert(max > 0)
      <=(max - 1)
    }
    def <=(max: Int) = {
      val parsers = for (n <- max to 0 by -1) yield p(n)
      parsers.reduceLeft((result, p) => result | p)
    }
  }

  // 5.1. Character Set
  val c_printable =
    0x09.p | 0xA.p | 0xD.p | (0x20 -> 0x7E) | /* 8 bit */
      0x85.p | (0xA0 -> 0xD7FF) | (0xE000 -> 0xFFD) | /* 16 bit */
      (0x10000 -> 0x10FFFF) /* 32 bit */

  val nb_json = 0x9.p | (0x20 -> 0x10FFFF)

  // 5.3. Indicator Characters
  val c_indicator = '-'.p | '?'.p | ':'.p | ','.p | '['.p | ']'.p | '{'.p | '}'.p |
    '#'.p | '&'.p | '*'.p | '!'.p | '|'.p | '>'.p | '\''.p | '"'.p |
    '%'.p | '@'.p | '`'.p

  val c_flow_indicator = ','.p | '['.p | ']'.p | '{'.p | '}'.p

  // 5.4. Line Break Characters
  val b_line_feed = 0xA.p /* LF */
  val b_carriage_return = 0xD.p /* CR */
  val b_char = b_line_feed | b_carriage_return
  val nb_char = c_printable - b_char
  val b_break = (b_carriage_return ~ b_line_feed) | /* DOS, Windows */
    b_carriage_return | /* MacOS upto 9.x */
    b_line_feed /* UNIX, MacOS X */
  val b_as_line_feed = b_break
  val b_non_content = b_break

  // 5.5. White Space Characters
  val ns_char = nb_char - s_white
  val s_space = 0x20.p /* SP */
  val s_tab = 0x09.p /* TAB */
  val s_white = s_space | s_tab

  // 5.6. Miscellaneous Characters
  val ns_dec_digit = (0x30 -> 0x39) /* 0-9 */
  val ns_hex_digit = ns_dec_digit | (0x41 -> 0x46) /* A-F */ | (0x61 -> 0x66) /* a-f */
  val ns_ascii_letter = (0x41 -> 0x5A) /* A-Z */ | (0x61 -> 0x7A) /* a-z */
  val ns_word_char = ns_dec_digit | ns_ascii_letter | '-'
  val ns_uri_char = ('%' ~ ns_hex_digit ~ ns_hex_digit) | ns_word_char | '#' |
    ';' | '/' | '?' | ':' | '@' | '&' | '=' | '+' | '$' | ',' |
    '_' | '.' | '!' | '~' | '*' | '\'' | '(' | ')' | '[' | ']'
  val ns_tag_char = ns_uri_char - '!' - c_flow_indicator

  val ns_esc_null = '0'.p
  val ns_esc_bell = 'a'.p
  val ns_esc_backspace = 'b'.p
  val ns_esc_horizontal_tab = ('t'.p | 0x9.p)
  val ns_esc_line_feed = 'n'.p
  val ns_esc_vertical_tab = 'v'.p
  val ns_esc_form_feed = 'f'.p
  val ns_esc_carriage_return = 'r'.p
  val ns_esc_escape = 'e'.p
  val ns_esc_space = 0x20.p
  val ns_esc_double_quote = '"'.p
  val ns_esc_slash = '/'.p
  val ns_esc_backslash = '\\'.p
  val ns_esc_next_line = 'N'.p
  val ns_esc_non_breaking_space = '_'.p
  val ns_esc_line_separator = 'L'.p
  val ns_esc_paragraph_separator = 'P'.p
  val ns_esc_8_bit = 'x' ~ (ns_hex_digit * 2)
  val ns_esc_16_bit = 'u' ~ (ns_hex_digit * 4)
  val ns_esc_32_bit = 'U' ~ (ns_hex_digit * 8)

  val c_ns_esc_char = '\\' ~
    (ns_esc_null | ns_esc_bell | ns_esc_backspace |
      ns_esc_horizontal_tab | ns_esc_line_feed |
      ns_esc_vertical_tab | ns_esc_form_feed |
      ns_esc_carriage_return | ns_esc_escape | ns_esc_space |
      ns_esc_double_quote | ns_esc_slash | ns_esc_backslash |
      ns_esc_next_line | ns_esc_non_breaking_space |
      ns_esc_line_separator | ns_esc_paragraph_separator |
      ns_esc_8_bit | ns_esc_16_bit | ns_esc_32_bit)

  // Chapter 6. Basic Structures
  // 6.1. Indentation Spaces
  def s_indent = (n: Int) => s_space * n

  // 6.2. Separation Spaces
  val s_separate_in_line = s_white.+ | StartOfLine

  // 6.3. Line Prefixes
  def s_line_prefix(n: Int, c: Context) =
    c match {
      case BlockOut => s_block_line_prefix(n)
      case BlockIn => s_block_line_prefix(n)
      case FlowOut => s_flow_line_prefix(n)
      case FlowIn => s_flow_line_prefix(n)
    }
  def s_block_line_prefix(n: Int) = s_indent(n)
  def s_flow_line_prefix(n: Int) = s_indent(n) ~ s_separate_in_line.?

  // 6.4. Empty Lines
  def l_empty(n: Int, c: Context) =
    (s_line_prefix(n, c) | (s_indent < n)) ~ b_as_line_feed

  // 6.5. Line Folding
  def b_l_trimmed(n: Int, c: Context) = b_non_content ~ l_empty(n, c).+
  val b_as_space = b_break
  def b_l_folded(n: Int, c: Context) = b_l_trimmed(n, c) | b_as_space

  def s_flow_folded(n: Int) = s_separate_in_line.? ~ b_l_folded(n, FlowIn) ~
    s_flow_line_prefix(n)

  // 6.6. Comments
  val c_nb_comment_text = '#' ~ nb_char.*
  val b_comment = b_non_content | EndOfFile
  val s_b_comment = (s_separate_in_line ~ c_nb_comment_text.?).? ~ b_comment
  val l_comment = s_separate_in_line ~ c_nb_comment_text.? ~ b_comment
  val s_l_comments = (s_b_comment | StartOfLine) ~ l_comment.*

  // 6.7. Separation Lines
  def s_separate(n: Int, c: Context) =
    c match {
      case BlockIn => s_separate_lines(n)
    }
  def s_separate_lines(n: Int) = (s_l_comments ~ s_flow_line_prefix(n)) |
    s_separate_in_line

  // 6.8. Directives
  val l_directive = '%' ~
    (ns_yaml_directive | ns_tag_directive | ns_reserved_directive) ~
    s_l_comments
  val ns_reserved_directive = ns_directive_name ~ (s_separate_in_line ~ ns_directive_parameter).*
  val ns_directive_name = ns_char.+
  val ns_directive_parameter = ns_char.+
  // 6.8.1. “YAML” Directives
  val ns_yaml_directive = 'Y' ~ 'A' ~ 'M' ~ 'L' ~ s_separate_in_line ~ ns_yaml_version
  val ns_yaml_version = ns_dec_digit.+ ~ '.' ~ ns_dec_digit.+
  // 6.8.2. “TAG” Directives
  val ns_tag_directive = 'T' ~ 'A' ~ 'G' ~
    s_separate_in_line ~ c_tag_handle ~ s_separate_in_line ~ ns_tag_prefix
  // 6.8.2.1. Tag Handles
  val c_tag_handle = c_named_tag_handle | c_secondary_tag_handle | c_primary_tag_handle
  val c_primary_tag_handle = '!'
  val c_secondary_tag_handle = '!' ~ '!'
  val c_named_tag_handle = '!' ~ ns_word_char.+ ~ '!'
  // 6.8.2.2. Tag Prefixes
  val ns_tag_prefix = c_ns_local_tag_prefix | ns_global_tag_prefix
  val c_ns_local_tag_prefix = '!' ~ ns_uri_char.*
  val ns_global_tag_prefix = ns_tag_char ~ ns_uri_char.*

  // 6.9. Node Properties
  def c_ns_properties(n: Int, c: Context) =
    (c_ns_tag_property ~
      (s_separate(n, c) ~ c_ns_anchor_property).?) |
      (c_ns_anchor_property ~
        (s_separate(n, c) ~ c_ns_tag_property).?)

  // 6.9.1. Node Tags
  val c_ns_tag_property = c_verbatim_tag |
    c_ns_shorthand_tag |
    c_non_specific_tag

  val c_verbatim_tag = '!' ~ '<' ~ ns_uri_char.+ ~ '>'

  val c_ns_shorthand_tag = c_tag_handle ~ ns_tag_char.+

  val c_non_specific_tag = '!'

  // 6.9.2. Node Anchors
  val c_ns_anchor_property = '&' ~ ns_anchor_name

  val ns_anchor_char = ns_char - c_flow_indicator
  val ns_anchor_name = ns_anchor_char.+

  // 7.1. Alias Nodes
  val c_ns_alias_node = '*' ~ ns_anchor_name

  // 7.2. Empty Nodes
  val e_scalar = Empty
  val e_node = e_scalar

  // 7.3. Flow Scalar Styles
  // 7.3.1. Double-Quoted Style
  val nb_double_char = c_ns_esc_char | (nb_json - '\\'.p - '"'.p)
  val ns_double_char = nb_double_char - s_white

  def c_double_quoted(n:Int,c:Context) = '"' ~ nb_double_text(n,c) ~ '"'
  def nb_double_text(n: Int, c: Context) =
    c match {
      case FlowOut => nb_double_multi_line(n)
      case FlowIn => nb_double_multi_line(n)
      case BlockKey => nb_double_one_line
      case FlowKey => nb_double_one_line
    }
  val nb_double_one_line = nb_double_char.*

  def s_double_escaped(n: Int) =
    s_white.* ~ '\\' ~ b_non_content ~
      l_empty(n, FlowIn).* ~ s_flow_line_prefix(n)
  def s_double_break(n: Int) = s_double_escaped(n) | s_flow_folded(n)

  val nb_ns_double_in_line = (s_white.* ~ ns_double_char).*
  def s_double_next_line(n: Int):Parser[_] =
    s_double_break(n) ~
      (ns_double_char ~ nb_ns_double_in_line ~
        (s_double_next_line(n) | s_white.*)).?
  def nb_double_multi_line(n: Int) =
    nb_ns_double_in_line ~
      (s_double_next_line(n) | s_white.*)

  // 7.3.2. Single-Quoted Style
  val c_quoted_quote = '\'' ~ '\''
  val nb_single_char = c_quoted_quote | (nb_json - '\'')
  val ns_single_char = nb_single_char - s_white

  def c_single_quoted(n: Int, c: Context) = '\'' ~ nb_single_text(n, c) ~ '\''
  def nb_single_text(n: Int, c: Context) =
    c match {
      case FlowOut => nb_single_multi_line(n)
      case FlowIn => nb_single_multi_line(n)
      case BlockKey => nb_single_one_line
      case FlowKey => nb_single_one_line
    }
  val nb_single_one_line = nb_single_char.*

  val nb_ns_single_in_line = (s_white.* ~ ns_single_char).*
  def s_single_next_line(n: Int): Parser[_] =
    s_flow_folded(n) ~
      (ns_single_char ~ nb_ns_single_in_line ~
        (s_single_next_line(n) | s_white.*)).?
  def nb_single_multi_line(n: Int) =
    nb_ns_single_in_line ~
      (s_single_next_line(n) | s_white.*)

  // 7.3.3. Plain Style
  def ns_plain_first(c: Context) = (ns_char - c_indicator) |
    (('?'.p | ':'.p | '-'.p) ~ guard(ns_plain_safe(c)))

  def ns_plain_safe(c: Context) =
    c match {
      case FlowOut => ns_plain_safe_out
      case FlowIn => ns_plain_safe_in
      case BlockKey => ns_plain_safe_out
      case FlowKey => ns_plain_safe_in
    }
  def ns_plain_safe_out = ns_char
  def ns_plain_safe_in = ns_char - c_flow_indicator
  def ns_plain_char(c: Context) = (ns_plain_safe(c) - ':'.p - '#'.p) |
    (ns_char ~ guard('#'.p)) |
    (':' ~ guard(ns_plain_safe(c)))

  def ns_plain(n: Int, c: Context) =
    c match {
      case FlowOut => ns_plain_multi_line(n, c)
      case FlowIn => ns_plain_multi_line(n, c)
      case BlockKey => ns_plain_one_line(c)
      case FlowKey => ns_plain_one_line(c)
    }
  def nb_ns_plain_in_line(c: Context) = (s_white.* ~ ns_plain_char(c)).*
  def ns_plain_one_line(c: Context) = ns_plain_first(c) ~ nb_ns_plain_in_line(c)

  def s_ns_plain_next_line(n: Int, c: Context) = s_flow_folded(n) ~
    ns_plain_char(c) ~ nb_ns_plain_in_line(c)
  def ns_plain_multi_line(n: Int, c: Context) = ns_plain_one_line(c) ~
    s_ns_plain_next_line(n, c).*

  // 7.4. Flow Collection Styles
  def in_flow(c: Context) =
    c match {
      case FlowOut => FlowIn
      case FlowIn => FlowIn
      case BlockKey => FlowKey
      case FlowKey => FlowKey
    }

  // 7.4.1. Flow Sequences
  def c_flow_sequence(n: Int, c: Context): Parser[_] = '[' ~ s_separate(n, c).? ~
    ns_s_flow_seq_entries(n, in_flow(c)).? ~ ']'

  def ns_s_flow_seq_entries(n: Int, c: Context):Parser[_] =
    ns_flow_seq_entry(n, c) ~ s_separate(n, c).? ~
    (',' ~ s_separate(n, c).? ~
      ns_s_flow_seq_entries(n, c).?).?

  def ns_flow_seq_entry(n: Int, c: Context) = ns_flow_pair(n, c) | ns_flow_node(n, c)

  // 7.4.2. Flow Mappings
  def c_flow_mapping(n: Int, c: Context) = '{' ~ s_separate(n, c).? ~
    ns_s_flow_map_entries(n, in_flow(c)).? ~ '}'

  def ns_s_flow_map_entries(n: Int, c: Context): Parser[_] =
    ns_flow_map_entry(n, c) ~ s_separate(n, c).? ~
      (',' ~ s_separate(n, c).? ~
        ns_s_flow_map_entries(n, c).?).?

  def ns_flow_map_entry(n: Int, c: Context) =
    ('?' ~ s_separate(n, c) ~
      ns_flow_map_explicit_entry(n, c)) |
      ns_flow_map_implicit_entry(n, c)
  def ns_flow_map_explicit_entry(n: Int, c: Context): Parser[_] =
    ns_flow_map_implicit_entry(n, c) |
      (e_node /* Key */ ~ e_node /* Value */ )

  def ns_flow_map_implicit_entry(n: Int, c: Context) =
    ns_flow_map_yaml_key_entry(n, c) |
      c_ns_flow_map_empty_key_entry(n, c) |
      c_ns_flow_map_json_key_entry(n, c)
  def ns_flow_map_yaml_key_entry(n: Int, c: Context): Parser[_] =
    ns_flow_yaml_node(n, c) ~
      ((s_separate(n, c).? ~
        c_ns_flow_map_separate_value(n, c)) |
        e_node)
  def c_ns_flow_map_empty_key_entry(n: Int, c: Context) = e_node /* Key */ ~
    c_ns_flow_map_separate_value(n, c)
  def c_ns_flow_map_separate_value(n: Int, c: Context): Parser[_] =
    ':' ~ not(ns_plain_safe(c)) ~
      ((s_separate(n, c) ~ ns_flow_node(n, c)) |
        e_node /* Value */ )

  def c_ns_flow_map_json_key_entry(n: Int, c: Context) =
    c_flow_json_node(n, c) ~
      ((s_separate(n, c).? ~
        c_ns_flow_map_adjacent_value(n, c)) |
        e_node)
  def c_ns_flow_map_adjacent_value(n: Int, c: Context): Parser[_] =
    ':' ~ ((s_separate(n, c).? ~
      ns_flow_node(n, c)) |
      e_node) /* Value */

  def ns_flow_pair(n: Int, c: Context) = ('?' ~ s_separate(n, c) ~
    ns_flow_map_explicit_entry(n, c)) |
    ns_flow_pair_entry(n, c)

  def ns_flow_pair_entry(n: Int, c: Context) =
    ns_flow_pair_yaml_key_entry(n, c) |
      c_ns_flow_map_empty_key_entry(n, c) |
      c_ns_flow_pair_json_key_entry(n, c)
  def ns_flow_pair_yaml_key_entry(n: Int, c: Context) =
    ns_s_implicit_yaml_key(FlowKey) ~
      c_ns_flow_map_separate_value(n, c)
  def c_ns_flow_pair_json_key_entry(n: Int, c: Context) =
    c_s_implicit_json_key(FlowKey) ~
      c_ns_flow_map_adjacent_value(n, c)
  def ns_s_implicit_yaml_key(c: Context) = ns_flow_yaml_node(na, c) ~ s_separate_in_line.?
  /* At most 1024 characters altogether */
  def c_s_implicit_json_key(c: Context) = c_flow_json_node(na, c) ~ s_separate_in_line.?
  /* At most 1024 characters altogether */

  // 7.5. Flow Nodes
  def ns_flow_yaml_content(n: Int, c: Context) = ns_plain(n, c)
  def c_flow_json_content(n: Int, c: Context) =
    c_flow_sequence(n, c) | c_flow_mapping(n, c) |
      c_single_quoted(n, c) | c_double_quoted(n, c)

  def ns_flow_content(n: Int, c: Context) =
    ns_flow_yaml_content(n, c) | c_flow_json_content(n, c)

  def ns_flow_yaml_node(n: Int, c: Context) = c_ns_alias_node |
    ns_flow_yaml_content(n, c) |
    (c_ns_properties(n, c) ~
      ((s_separate(n, c) ~
        ns_flow_yaml_content(n, c)) |
        e_scalar))
  def c_flow_json_node(n: Int, c: Context): Parser[_] =
    (c_ns_properties(n, c) ~ s_separate(n, c)).? ~
      c_flow_json_content(n, c)
  def ns_flow_node(n: Int, c: Context) = c_ns_alias_node |
    ns_flow_content(n, c) |
    (c_ns_properties(n, c) ~
      ((s_separate(n, c) ~
        ns_flow_content(n, c)) |
        e_scalar))

  // 8.1. Block Scalar Styles
  // 8.1.1. Block Scalar Headers
  def c_b_block_header = new {
    def ~[U](innerParser: (Int, Chomping) => Parser[U]) = {

      val c_b_block_header =
        ((c_indentation_indicator ~ c_chomping_indicator) ^^ { case m ~ t => (m, t) }
          | (c_chomping_indicator ~ c_indentation_indicator) ^^ { case t ~ m => (m, t) }) ~ s_b_comment

      c_b_block_header >> {
        case (m, t) ~ comment =>
          innerParser(m, t).map(inner => new ~(comment, inner)).named("~")
      }
    }
  }

  // 8.1.1.1. Block Indentation Indicator
  val c_indentation_indicator = ((ns_dec_digit - 0x30.p) | Empty) >> {
    case Empty => AutoDetectIndentation
    case i: Char => success(i.asDigit)
  }

  // 8.1.1.2. Block Chomping Indicator
  val c_chomping_indicator = ('-'.p | '+'.p | Empty) ^^ {
    case '-' => Strip
    case '+' => Keep
    case Empty => Clip
  }
  def b_chomped_last(t: Chomping) =
    t match {
      case Strip => b_non_content | EndOfFile
      case Clip => b_as_line_feed | EndOfFile
      case Keep => b_as_line_feed | EndOfFile
    }
  def l_chomped_empty(n: Int, t: Chomping) =
    t match {
      case Strip => l_strip_empty(n)
      case Clip => l_strip_empty(n)
      case Keep => l_keep_empty(n)
    }
  def l_strip_empty(n: Int) = ((s_indent <= n) ~ b_non_content).* ~
    l_trail_comments(n).?
  def l_keep_empty(n: Int) = l_empty(n, BlockIn).* ~
    l_trail_comments(n).?
  def l_trail_comments(n: Int) = (s_indent < n) ~ c_nb_comment_text ~ b_comment ~
    l_comment.*

  // 8.1.2. Literal Style
  def c_l_literal(n: Int) = '|' ~ (c_b_block_header ~ { (m, t) =>
    l_literal_content(n + m, t)
  })

  def l_nb_literal_text(n: Int) = l_empty(n, BlockIn).* ~
    s_indent(n) ~ nb_char.+
  def b_nb_literal_next(n: Int) = b_as_line_feed ~ l_nb_literal_text(n)
  def l_literal_content(n: Int, t: Chomping) =
    (l_nb_literal_text(n) ~ b_nb_literal_next(n).* ~ b_chomped_last(t)).? ~
      l_chomped_empty(n, t)

  // 8.1.3. Folded Style
  def c_l_folded(n: Int) = '>' ~ (c_b_block_header ~ { (m, t) =>
    l_folded_content(n + m, t)
  })

  def s_nb_folded_text(n: Int) = s_indent(n) ~ ns_char ~ nb_char.*
  def l_nb_folded_lines(n: Int) = s_nb_folded_text(n) ~
    (b_l_folded(n, BlockIn) ~ s_nb_folded_text(n)).*

  def s_nb_spaced_text(n: Int) = s_indent(n) ~ s_white ~ nb_char.*
  def b_l_spaced(n: Int) = b_as_line_feed ~
    l_empty(n, BlockIn).*
  def l_nb_spaced_lines(n: Int) = s_nb_spaced_text(n) ~
    (b_l_spaced(n) ~ s_nb_spaced_text(n)).*

  def l_nb_same_lines(n: Int) = l_empty(n, BlockIn).* ~
    (l_nb_folded_lines(n) | l_nb_spaced_lines(n))
  def l_nb_diff_lines(n: Int) = l_nb_same_lines(n) ~
    (b_as_line_feed ~ l_nb_same_lines(n)).*

  def l_folded_content(n: Int, t: Chomping) =
    (l_nb_diff_lines(n) ~ b_chomped_last(t)).? ~
      l_chomped_empty(n, t)

  // 8.2.1. Block Sequences
  def l_block_sequence(n: Int) =
    AutoDetectIndentation >> { detected =>
      val m = detected - n
      if (m > 0)
        (s_indent(n + m) ~ c_l_block_seq_entry(n + m)).+
      else
        failure("Expected an indentation of at least" + (n + 1))
    }
  def c_l_block_seq_entry(n: Int): Parser[_] = '-' ~ not(ns_char) ~ s_l_block_indented(n, BlockIn)

  def s_l_block_indented(n: Int, c: Context): Parser[_] =
    AutoDetectIndentation >> { detected =>
      val m = detected - n

      (s_indent(m) ~
        (ns_l_compact_sequence(n + 1 + m) |
          ns_l_compact_mapping(n + 1 + m))) |
          s_l_block_node(n, c) |
          (e_node ~ s_l_comments)

    }
  def ns_l_compact_sequence(n: Int) = c_l_block_seq_entry(n) ~
    (s_indent(n) ~ c_l_block_seq_entry(n)).*

  // 8.2.2. Block Mappings
  def l_block_mapping(n: Int) =
    AutoDetectIndentation >> { detected =>
      val m = detected - n
      if (m > 0)
        (s_indent(n + m) ~ ns_l_block_map_entry(n + m)).+
      else
        failure("Expected an indentation of at least" + (n + 1))
    }

  def ns_l_block_map_entry(n: Int) = c_l_block_map_explicit_entry(n) |
    ns_l_block_map_implicit_entry(n)
  def c_l_block_map_explicit_entry(n: Int) = c_l_block_map_explicit_key(n) ~
    (l_block_map_explicit_value(n)
      | e_node)
  def c_l_block_map_explicit_key(n: Int) = '?' ~ s_l_block_indented(n, BlockOut)
  def l_block_map_explicit_value(n: Int) = s_indent(n) ~
    ':' ~ s_l_block_indented(n, BlockOut)

  def ns_l_block_map_implicit_entry(n: Int) = (ns_s_block_map_implicit_key
    | e_node) ~
    c_l_block_map_implicit_value(n)
  def ns_s_block_map_implicit_key = c_s_implicit_json_key(BlockKey) |
    ns_s_implicit_yaml_key(BlockKey)

  def c_l_block_map_implicit_value(n: Int) = ':' ~ (s_l_block_node(n, BlockOut) |
    (e_node ~ s_l_comments))

  def ns_l_compact_mapping(n: Int) = ns_l_block_map_entry(n) ~
    (s_indent(n) ~ ns_l_block_map_entry(n)).*

  // 8.2.3. Block Nodes
  def s_l_block_node(n: Int, c: Context): Parser[_] =
    s_l_block_in_block(n, c) | s_l_flow_in_block(n)
  def s_l_flow_in_block(n: Int) = s_separate(n + 1, FlowOut) ~
    ns_flow_node(n + 1, FlowOut) ~ s_l_comments

  def s_l_block_in_block(n: Int, c: Context) =
    s_l_block_scalar(n, c) | s_l_block_collection(n, c)
  def s_l_block_scalar(n: Int, c: Context) = s_separate(n + 1, c) ~
    (c_ns_properties(n + 1, c) ~ s_separate(n + 1, c)).? ~
    (c_l_literal(n) | c_l_folded(n))

  def s_l_block_collection(n: Int, c: Context) =
    (s_separate(n + 1, c) ~ c_ns_properties(n + 1, c)).? ~
      s_l_comments ~
      (l_block_sequence(seq_spaces(n, c)) | l_block_mapping(n))
  def seq_spaces(n: Int, c: Context) =
    c match {
      case BlockOut => n - 1
      case BlockIn => n
      case unexpected => sys.error("Unexpected context in seq_spaces: " + unexpected)
    }

  // Chapter 9. YAML Character Stream

  // 9.1. Documents
  // 9.1.1. Document Prefix
  val l_document_prefix = l_comment.*
  // 9.1.2. Document Markers
  val c_directives_end = '-' ~ '-' ~ '-'
  val c_document_end = '.' ~ '.' ~ '.'
  val l_document_suffix = c_document_end ~ s_l_comments
  // 9.1.3. Bare Documents
  val l_bare_document = s_l_block_node(-1, BlockIn)
  // 9.1.4. Explicit Documents
  val l_explicit_document = c_directives_end ~ (l_bare_document | (e_node ~ s_l_comments))
  // 9.1.5. Directives Documents
  val l_directive_document = l_directive.+ ~ l_explicit_document

  // 9.2. Streams
  val l_any_document =
    l_directive_document | l_explicit_document | l_bare_document

  val l_yaml_stream =
    l_document_prefix.* ~ l_any_document.? ~
      (l_document_suffix.+ ~ l_document_prefix.* ~ l_any_document.?
        | l_document_prefix.* ~ l_explicit_document.?).*
}