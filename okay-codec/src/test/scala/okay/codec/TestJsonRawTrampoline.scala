package okay.codec

/**
 * `JsonValue.Parser.value`/`obj`/`arr` and `Json.into`/`pairs`'
 * threshold split (json-raw-nesting-threshold-trampoline,
 * specs/iterative-recursive-decode.md, targets 1+2): native recursion
 * below `Codecs.NativeThreshold`, `Cont.defer` past it — the raw JSON
 * container-nesting walks, no schema involved. `Codecs.maxDepth` (the
 * cap both roads used to refuse past) is gone (remove-codecs-maxdepth):
 * neither road cuts any more, at any depth.
 */
class TestJsonRawTrampoline extends munit.FunSuite:

  // a genuinely deep document can legitimately take longer than
  // munit's 30s default under a loaded gate box (many suites running
  // at once) — this is correctness, not a performance test
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  def arrays(d: Int): String = ("[" * d) + "1" + ("]" * d)
  def objects(d: Int): String = ("{\"a\":" * d) + "1" + ("}" * d)

  def depth(j: Json): Int =
    var d = 0
    var at = j
    var go = true
    while go do at match
      case Json.JArr(Vector(one)) => d += 1; at = one
      case Json.JObj(Vector((_, one))) => d += 1; at = one
      case _ => go = false
    d

  /** structural equality on a 100 000-deep tree is its OWN
    * StackOverflowError (`Vector.sameElements`/`JArr.equals` recurse) —
    * unrelated to the trampoline under test — so compare node-by-node
    * in a loop instead of `assertEquals` on the whole tree */
  def sameChain(a: Json, b: Json): Boolean =
    var x = a
    var y = b
    var ok = true
    var go = true
    while go do
      (x, y) match
        case (Json.JArr(Vector(one)), Json.JArr(Vector(two))) => x = one; y = two
        case (Json.JObj(Vector((k1, one))), Json.JObj(Vector((k2, two)))) =>
          if k1 != k2 then { ok = false; go = false } else { x = one; y = two }
        case (Json.JNum(n1), Json.JNum(n2)) => ok = n1 == n2; go = false
        case _ => ok = false; go = false
    ok

  test("at a genuinely large depth, both roads still answer the same value, arrays and objects") {
    val n = 100000
    val a = arrays(n)
    assert(JsonValue.parse(a).isDefined, "the fast road refused a well-formed deep array")
    val va = Json.parse(a)
    assert(sameChain(va, Json.lossless(a)), "Json.parse and Json.lossless disagree")
    assertEquals(depth(va), n)

    val o = objects(n)
    assert(JsonValue.parse(o).isDefined, "the fast road refused a well-formed deep object")
    val vo = Json.parse(o)
    assert(sameChain(vo, Json.lossless(o)), "Json.parse and Json.lossless disagree")
    assertEquals(depth(vo), n)
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
