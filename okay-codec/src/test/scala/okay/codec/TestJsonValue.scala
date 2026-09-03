package okay.codec

import Json.*

/**
 * specs/codecs.md, "Value parser": the fast road and the lossless
 * road yield ONE Json — equal values on every well-formed document,
 * and on every damaged one, because the fast road refuses and the
 * lossless road answers. The prefix sweep is the strong form: every
 * truncation of every corpus document, both roads, equal.
 */
class TestJsonValue extends munit.FunSuite {

  val wellFormed = Seq(
    """{"id":42,"user":"ada","amount":12.5,"active":true,"tags":["new","vip"],"addr":{"city":"Kyiv","zip":"01001","line":null},"note":"leave at door"}""",
    "{}", "[]", "0", "-0", "1", "-1.5e3", "1E+2", "1e-2", "123456789012345678", "1e999", "-1e999",
    "\"\"", "\"plain\"", "\"q\\\"uo\\\\te\\n\\t\\r\"", "\"\\u0041\"", "\"\\b\\f\\/\"", "\"日本語 ünïcödé\"",
    "true", "false", "null",
    " \n\t [ 1 , 2 , 3 ] \r\n", "{ \"a\" : { \"b\" : [ { } , [ ] , null ] } }",
    """{"a":1,"a":2}""", """[[[[[[1]]]]]]""", """{"k":"v","n":-0.0,"e":1.0E10}""",
    """[1,"two",true,null,{"three":3},[4]]""")

  val damaged = Seq(
    "", "   ", "-", "[1,2,-", "{\"a\":-}", "-e5", "1e", ".", "+", "01", "1.", ".5", "1.e5",
    "[1,2,]", "{\"a\":1,}", "{\"a\"}", "{a:1}", "{'a':1}", "[1 2]", "1 2", "[1]]", "{\"a\":1}}",
    "\"unterminated", "\"raw\ncontrol\"", "\"tab\there\"", "tru", "nul", "[tru]", "{\"a\":nulls}",
    "\"esc at end\\", "[\"a\",]", "{\"a\":[1,2}", "[{\"a\":1]", "\u0000", "[1,2,3", "{\"a\":\"b\"")

  def same(s: String)(using munit.Location): Unit =
    assertEquals(Json.parseValue(s), Json.parse(s), s"disagree on <$s>")

  test("well-formed documents: the fast road answers, and equals the lossless one") {
    wellFormed.foreach { s =>
      assert(JsonValue.parse(s).isDefined, s"the fast road refused a well-formed document <$s>")
      same(s)
    }
  }

  test("damaged documents: the fast road refuses, the lossless one answers, parseValue is that answer") {
    damaged.foreach { s =>
      assert(JsonValue.parse(s).isEmpty, s"the fast road accepted damage <$s>")
      same(s)
    }
  }

  test("the prefix sweep: every truncation of every document, both roads equal") {
    (wellFormed ++ damaged).foreach { s =>
      (0 to s.length).foreach(k => same(s.take(k)))
    }
  }

  test("the projection's escape reading: \\u names a code point now, \\b is still b") {
    // json-unicode-escape (2026-09-03): \u used to decode to the four
    // literal letters that named it — "u0041" — because the catch-all
    // single-character-escape case handled it the same as \b. \b keeps
    // its non-RFC reading (this project's own choice, unrelated to the
    // bug); \u no longer shares its fate.
    assertEquals(JsonValue.parse("\"\\u0041\\b\""), Some(JStr("Ab")))
    assertEquals(JsonValue.parse("\"a\\nb\""), Some(JStr("a\nb")))
  }

  test("a surrogate pair reconstructs the one character it names") {
    // U+1F600, GRINNING FACE — outside the BMP, so JSON (and this
    // parser) sees it as two \u escapes; two appended UTF-16 code
    // units that form a valid pair are already a correct String,
    // needing no pairing logic of this parser's own
    assertEquals(JsonValue.parse("\"\\ud83d\\ude00\""), Some(JStr("😀")))
    assertEquals(Json.parse("\"\\ud83d\\ude00\""), JStr("😀"))
  }

  test("Cyrillic survives an escaping JSON producer, not just a raw one") {
    // the case that found the bug: Telegram's own Bot API escapes
    // non-ASCII, and every character of it was coming through as
    // garbage before this fix
    assertEquals(JsonValue.parse("\"\\u043d\\u0443\\u0436\\u043d\\u0430\""), Some(JStr("нужна")))
    assertEquals(Json.parse("\"\\u043d\\u0443\\u0436\\u043d\\u0430\""), JStr("нужна"))
  }

  test("a truncated escape at the end of a string does not throw") {
    // fewer than four hex digits before the closing quote: damage,
    // answered the same wrong-but-safe way every other case here is —
    // unquote returns a bare String, so there is no JErr to become,
    // and the parser falls back to the un-decoded single-char reading
    assertEquals(Json.parse("\"ab\\u12\""), JStr("abu12"))
    assertEquals(JsonValue.parse("\"ab\\u12\""), Some(JStr("abu12")))
  }

  test("numbers are toDouble's, including the edges") {
    assertEquals(JsonValue.parse("1e999"), Some(JNum(Double.PositiveInfinity)))
    assertEquals(JsonValue.parse("-0"), Some(JNum(-0.0)))
    assertEquals(JsonValue.parse("123456789012345678"), Some(JNum(1.23456789012345678e17)))
  }
}
