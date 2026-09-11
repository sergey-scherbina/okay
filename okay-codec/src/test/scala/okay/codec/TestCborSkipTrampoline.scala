package okay.codec

/**
 * `Cbor.In.skipItem`'s threshold split (cbor-skip-threshold-trampoline,
 * specs/iterative-recursive-decode.md, target 4): native recursion
 * below `Cbor.NativeThreshold`, `Cont.defer` past it, the same design
 * `Cbor.get` already carries (`TestCborTrampoline`) — but for skipping
 * an UNDECLARED field's value rather than decoding a declared one.
 */
class TestCborSkipTrampoline extends munit.FunSuite:

  // a genuinely deep document can legitimately take longer than
  // munit's 30s default under a loaded gate box (many suites running
  // at once) — this is correctness, not a performance test
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  final case class OnlyA(a: String)
  given Schema[OnlyA] = Schema.derived

  /** an OnlyA-shaped frame whose "deep" field is n nested one-element
    * arrays around a leaf integer — undeclared, so it is SKIPPED */
  def frameWithDeepUnknown(n: Int): Array[Byte] =
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("a"); out.text("kept")
    out.text("deep")
    for _ <- 0 until n do out.arrayHeader(1)
    out.integer(1)
    out.toArray

  test("a deeply nested UNDECLARED field, past NativeThreshold, is still skipped correctly") {
    val n = 40   // comfortably past NativeThreshold (24), exercising the switch
    Cbor.read[OnlyA](frameWithDeepUnknown(n)) match
      case Left(e) => fail(s"expected the unknown field skipped, got: $e")
      case Right(v) => assertEquals(v, OnlyA("kept"))
  }

  test("a genuinely deep UNDECLARED field skips correctly — no cap (remove-codecs-maxdepth)") {
    // this depth used to be refused outright (Codecs.maxDepth); once
    // skipItem trampolined past NativeThreshold, the cap had no
    // remaining job, and this document decodes instead of refusing
    Cbor.read[OnlyA](frameWithDeepUnknown(200000)) match
      case Left(e) => fail(s"expected the unknown field skipped, got: $e")
      case Right(v) => assertEquals(v, OnlyA("kept"))
  }

  test("a truncated unknown field past the threshold is still damage, not a silent skip") {
    val whole = frameWithDeepUnknown(40)
    val cut = whole.take(whole.length - 3)
    assert(Cbor.read[OnlyA](cut).isLeft, "a cut-off deep unknown field must not read as a successful skip")
  }

  test("ordinary shallow unknown fields (every major type) are unaffected") {
    final case class Known(a: Int, z: String)
    given Schema[Known] = Schema.derived
    final case class WithInt(a: Int, skipped: Long, z: String)
    given Schema[WithInt] = Schema.derived
    assertEquals(Cbor.read[Known](Cbor.write(WithInt(1, 7, "end"))), Right(Known(1, "end")))
  }
