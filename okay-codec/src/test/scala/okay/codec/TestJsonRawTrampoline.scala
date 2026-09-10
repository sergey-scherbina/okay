package okay.codec

/**
 * `JsonValue.Parser.value`/`obj`/`arr` and `Json.into`/`pairs`'
 * threshold split (json-raw-nesting-threshold-trampoline,
 * specs/iterative-recursive-decode.md, targets 1+2): native recursion
 * below `Codecs.NativeThreshold`, `Cont.defer` past it — the raw JSON
 * container-nesting walks, no schema involved.
 */
class TestJsonRawTrampoline extends munit.FunSuite:

  def arrays(d: Int): String = ("[" * d) + "1" + ("]" * d)
  def objects(d: Int): String = ("{\"a\":" * d) + "1" + ("}" * d)

  test("at Codecs.maxDepth, both roads still answer the same value, arrays and objects") {
    val a = arrays(Codecs.maxDepth)
    assertEquals(JsonValue.parse(a).isDefined, true)
    assertEquals(Json.parse(a), Json.lossless(a))
    val o = objects(Codecs.maxDepth)
    assertEquals(JsonValue.parse(o).isDefined, true)
    assertEquals(Json.parse(o), Json.lossless(o))
  }

  test("past Codecs.maxDepth, the fast road answers None and the lossless road cuts, both past NativeThreshold") {
    val a = arrays(Codecs.maxDepth + 1)
    assertEquals(JsonValue.parse(a), None)
    assert(Json.isCut(Json.lossless(a)))
    assertEquals(Json.parse(a), Json.lossless(a))
  }

  test("ordinary shallow documents (below the threshold) are unaffected") {
    val samples = List(
      "{\"a\": [1, 2.5e3, true],\n \"b\": null}",
      "[1, [2, 3], {\"x\": \"y\"}]",
      "\"just a string\"",
      "42",
    )
    for s <- samples do
      assertEquals(Json.parse(s), Json.lossless(s), s"disagree on <$s>")
      assert(JsonValue.parse(s).isDefined, s"the fast road refused <$s>")
  }
