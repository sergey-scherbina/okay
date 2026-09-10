package okay.codec

/**
 * `Cbor.In.skipItem`'s threshold split (cbor-skip-threshold-trampoline,
 * specs/iterative-recursive-decode.md, target 4): native recursion
 * below `Cbor.NativeThreshold`, `Cont.defer` past it, the same design
 * `Cbor.get` already carries (`TestCborTrampoline`) — but for skipping
 * an UNDECLARED field's value rather than decoding a declared one.
 */
class TestCborSkipTrampoline extends munit.FunSuite:

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

  test("a deeply nested UNDECLARED field, far past NativeThreshold, is still skipped correctly") {
    // 40 is comfortably past NativeThreshold (24) and comfortably
    // under Codecs.maxDepth (64), so the switch is exercised without
    // hitting the wire-contract refusal
    val n = 40
    Cbor.read[OnlyA](frameWithDeepUnknown(n)) match
      case Left(e) => fail(s"expected the unknown field skipped, got: $e")
      case Right(v) => assertEquals(v, OnlyA("kept"))
  }

  test("Codecs.maxDepth still refuses a skip nested past it, on either side of the threshold") {
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("a"); out.text("kept")
    out.text("deep")
    for _ <- 0 to Codecs.maxDepth + 1 do out.arrayHeader(1)
    out.integer(1)
    Cbor.read[OnlyA](out.toArray) match
      case Left(e) => assert(e.contains(s"nested deeper than ${Codecs.maxDepth}"), e)
      case Right(v) => fail(s"decoded $v past Codecs.maxDepth through an unknown field")
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
