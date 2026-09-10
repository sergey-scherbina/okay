package okay.codec

/**
 * `Cbor.get`'s threshold split (cbor-decode-threshold-trampoline,
 * specs/iterative-recursive-decode.md): native recursion below
 * `NativeThreshold`, `Cont.defer` past it, so a recursive schema's
 * depth stops costing native stack once it matters and
 * `Codecs.maxDepth` can be raised without reopening
 * lower-maxdepth-real-margin's tradeoff.
 */
class TestCborTrampoline extends munit.FunSuite:

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def cborChain(n: Int): Array[Byte] =
    val out = new Cbor.Out
    for _ <- 0 until n do { out.mapHeader(1); out.text("kids"); out.arrayHeader(1) }
    out.mapHeader(1); out.text("kids"); out.arrayHeader(0)
    out.toArray

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d

  test("at Codecs.maxDepth (past NativeThreshold), the value is still correct") {
    // Codecs.maxDepth (64) is still the WIRE-CONTRACT limit this lane
    // does not touch — a document deeper than it refuses regardless
    // of what the decoder's own stack cost would have been, so this
    // proves the trampoline gives the RIGHT VALUE at the deepest
    // depth the wire still allows (well past NativeThreshold=24, so
    // the switch is exercised), not that the wire limit is gone
    // two containers (map + array) per tree LEVEL, so this is the
    // deepest tree Codecs.maxDepth still allows through
    val n = Codecs.maxDepth / 2 - 1
    val bytes = cborChain(n)
    Cbor.read[Tree](bytes) match
      case Left(e) => fail(s"expected a value at exactly the limit, got: $e")
      case Right(t) => assertEquals(depthOf(t), n + 1)
  }

  test("stack safety past maxDepth is a claim about the DECODER, not visible through it yet") {
    // this lane's own scope: Codecs.maxDepth stays 64 here (raising it
    // is a follow-up decision once this lane's numbers are in, per the
    // claim) — so a document deeper than 64 refuses at the wire-limit
    // check BEFORE the trampoline's own depth capability is what is
    // being asked. TestStackBytes's "cost is now FLAT past the
    // threshold" is where the mechanism itself is proved (it measures
    // Cbor.get's stack cost directly, independent of what maxDepth
    // happens to allow through); this test records why a 200 000-level
    // document is not the right test HERE, so nobody re-adds it
    // expecting it to pass before maxDepth moves.
    val bytes = cborChain(200000)
    Cbor.read[Tree](bytes) match
      case Left(e) => assert(e.contains(s"nested deeper than ${Codecs.maxDepth}"), e)
      case Right(_) => fail("expected the wire-contract limit to refuse this, independent of the trampoline")
  }

  test("ordinary shapes below the threshold are untouched: products, sums, lists, options, isos") {
    enum Color derives Schema:
      case Red
      case Green(shade: Int)
    final case class Email(value: String)
    given Schema[Email] = Schema.wrap[Email, String](Email(_), _.value)
    final case class Order(id: Int, tags: List[String], color: Color,
                            email: Option[Email], amounts: Vector[Double])
    given Schema[Order] = Schema.derived

    val o = Order(7, List("a", "b"), Color.Green(3), Some(Email("x@y.z")), Vector(1.5, -2.0))
    assertEquals(Cbor.read[Order](Cbor.write(o)), Right(o))
    assertEquals(Cbor.read[Order](Cbor.write(o.copy(email = None))), Right(o.copy(email = None)))
  }

  test("errors past the threshold still refuse, not silently succeed") {
    final case class Bad(a: Int)
    given Schema[Bad] = Schema.derived
    // a chain of Trees where the schema at the bottom does not match —
    // Bad wants an Int where the Tree chain's leaf is an empty array
    val bytes = cborChain(50)
    assert(Cbor.read[Bad](bytes).isLeft)
  }

  test("Codecs.maxDepth is still enforced past the native threshold") {
    final case class OnlyA(a: String)
    given Schema[OnlyA] = Schema.derived
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("a"); out.text("kept")
    out.text("deep")
    for _ <- 0 to Codecs.maxDepth + 1 do out.arrayHeader(1)
    out.integer(1)
    Cbor.read[OnlyA](out.toArray) match
      case Left(e) => assert(e.contains(s"nested deeper than ${Codecs.maxDepth}"), e)
      case Right(v) => fail(s"decoded $v past Codecs.maxDepth")
  }
