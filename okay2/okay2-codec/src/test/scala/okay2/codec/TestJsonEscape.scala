package okay2.codec

/** What `Json.escape` escapes, pinned, and escape/read agreeing as a
 * round trip (okay-codec's TestJsonEscape). */
class TestJsonEscape extends munit.FunSuite {

  test("exactly five characters are escaped, and no others") {
    assertEquals(Json.escape("\""), "\\\"")
    assertEquals(Json.escape("\\"), "\\\\")
    assertEquals(Json.escape("\n"), "\\n")
    assertEquals(Json.escape("\t"), "\\t")
    assertEquals(Json.escape("\r"), "\\r")
  }

  test("the ones deliberately NOT escaped stay themselves") {
    val untouched = Vector('\b', '\f', ' ', '\u0001', '\u001f', '/', 'é', '日')
    for (c <- untouched) assertEquals(Json.escape(c.toString), c.toString, s"escaped U+${c.toInt.toHexString}")
  }

  test("a string with nothing to escape comes back equal") {
    for (s <- Vector("", "plain", "a b c", "日本語 ünïcödé", "0123456789", "/slash/es/")) assertEquals(Json.escape(s), s)
  }

  test("mixed strings: every occurrence, at the edges and in the middle") {
    assertEquals(Json.escape("a\"b"), "a\\\"b")
    assertEquals(Json.escape("\"lead"), "\\\"lead")
    assertEquals(Json.escape("trail\""), "trail\\\"")
    assertEquals(Json.escape("\"\"\""), "\\\"\\\"\\\"")
    assertEquals(Json.escape("a\\b\nc\td\re"), "a\\\\b\\nc\\td\\re")
    assertEquals(Json.escape("\n\r\t"), "\\n\\r\\t")
  }

  test("escape then read back is the identity, on both roads") {
    val corpus = Vector(
      "", "plain", "a\"b", "back\\slash", "line\nbreak", "tab\there", "ret\rurn",
      "all five: \" \\ \n \t \r", "日本語", "\b\f", "/", "\\\\", "\"\"",
      "ends with backslash \\", "\\n is two characters, not a newline")
    for (s <- corpus) {
      val text = Json.print(Json.JStr(s))
      assertEquals(Json.parse(text), Json.JStr(s), s"round trip lost <$s> (as $text)")
      assertEquals(Json.lossless(text), Json.JStr(s), s"the lossless road disagrees on <$s>")
    }
  }

  test("a key is escaped the same way a value is") {
    val j = Json.JObj(Vector("a\"b\nc" -> Json.JStr("v")))
    assertEquals(Json.parse(Json.print(j)), j)
  }
}
