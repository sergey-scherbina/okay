package okay2.codec

import Json._

/** The fast road and the lossless road yield ONE Json: equal values on
 * every well-formed document, and on every damaged one, because the
 * fast road refuses and the lossless road answers (okay-codec's
 * TestJsonValue). The prefix sweep is the strong form. */
class TestJsonValue extends munit.FunSuite {

  import JsonCorpus.{wellFormed, damaged}

  def same(s: String)(implicit loc: munit.Location): Unit =
    assertEquals(Json.parse(s), Json.lossless(s), s"disagree on <$s>")

  test("well-formed documents: the fast road answers, and equals the lossless one") {
    wellFormed.foreach { s =>
      assert(JsonValue.parse(s).isDefined, s"the fast road refused a well-formed document <$s>")
      same(s)
    }
  }

  test("damaged documents: the fast road refuses, the lossless one answers, and parse is that answer") {
    damaged.foreach { s =>
      assert(JsonValue.parse(s).isEmpty, s"the fast road accepted damage <$s>")
      same(s)
    }
  }

  test("the prefix sweep: every truncation of every document, both roads equal") {
    (wellFormed ++ damaged).foreach(s => (0 to s.length).foreach(k => same(s.take(k))))
  }

  test("the projection's escape reading: \\u names a code point, \\b is b") {
    assertEquals(JsonValue.parse("\"\\u0041\\b\""), Some(JStr("Ab")))
    assertEquals(JsonValue.parse("\"a\\nb\""), Some(JStr("a\nb")))
  }

  test("a surrogate pair reconstructs the one character it names") {
    assertEquals(JsonValue.parse("\"\\ud83d\\ude00\""), Some(JStr("😀")))
    assertEquals(Json.parse("\"\\ud83d\\ude00\""), JStr("😀"))
  }

  test("Cyrillic survives an escaping JSON producer") {
    assertEquals(JsonValue.parse("\"\\u043d\\u0443\\u0436\\u043d\\u0430\""), Some(JStr("нужна")))
    assertEquals(Json.parse("\"\\u043d\\u0443\\u0436\\u043d\\u0430\""), JStr("нужна"))
  }

  test("a truncated escape at the end of a string does not throw") {
    assertEquals(Json.parse("\"ab\\u12\""), JStr("abu12"))
    assertEquals(JsonValue.parse("\"ab\\u12\""), Some(JStr("abu12")))
  }

  test("numbers are parseDouble's, including the edges") {
    assertEquals(JsonValue.parse("1e999"), Some(JNum(Double.PositiveInfinity)))
    assertEquals(JsonValue.parse("-0"), Some(JNum(-0.0)))
    assertEquals(JsonValue.parse("123456789012345678"), Some(JNum(1.23456789012345678e17)))
  }
}
