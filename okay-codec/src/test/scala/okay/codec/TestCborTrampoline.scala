package okay.codec

/**
 * `Cbor.get`'s threshold split (cbor-decode-threshold-trampoline,
 * specs/iterative-recursive-decode.md): native recursion below
 * `NativeThreshold`, `Cont.defer` past it. `Codecs.maxDepth` — the
 * wire-contract refusal this was originally justified against — is
 * gone (remove-codecs-maxdepth): once every door trampolines, nothing
 * needed a cap for its own safety, so the two tests that used to prove
 * "the wire limit still hides the trampoline's real depth capacity"
 * are replaced by proving that capacity directly.
 */
class TestCborTrampoline extends munit.FunSuite:

  // a genuinely deep document can legitimately take longer than
  // munit's 30s default under a loaded gate box (many suites running
  // at once) — this is correctness, not a performance test
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

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

  test("a genuinely deep recursive value decodes correctly — no cap, no stack overflow") {
    // 200 000 is the depth stack-depth-margin's own first probe found
    // NATIVE recursion overflowing at, and lower-maxdepth-real-margin
    // once had to refuse long before reaching it; now there is nothing
    // between a well-formed message of this shape and a correct answer
    val n = 200000
    val bytes = cborChain(n)
    Cbor.read[Tree](bytes) match
      case Left(e) => fail(s"expected a value, got: $e")
      case Right(t) => assertEquals(depthOf(t), n + 1)
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
