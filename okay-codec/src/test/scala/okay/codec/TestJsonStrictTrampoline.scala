package okay.codec

/**
 * `JsonStrict.Reader.get`'s threshold split (jsonstrict-threshold-
 * trampoline, specs/iterative-recursive-decode.md, target 3 — the
 * last of four): native recursion below `Codecs.NativeThreshold`,
 * `Cont.defer` past it, the same design `Cbor.get`/`Json.decode`
 * already carry. `Codecs.maxDepth` — the wire-contract refusal this
 * was once measured against — is gone (remove-codecs-maxdepth): a
 * document that used to be refused now decodes.
 */
class TestJsonStrictTrampoline extends munit.FunSuite:

  // a genuinely deep document can legitimately take longer than
  // munit's 30s default under a loaded gate box (many suites running
  // at once) — this is correctness, not a performance test
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  /** past `Codecs.NativeThreshold` (24), comfortably shallow — just
    * enough to exercise the switch without the cost of a huge document */
  val PastThreshold = 40

  def jsonChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d

  test("past NativeThreshold, readStrict answers correctly") {
    JsonStrict.read[Tree](jsonChain(PastThreshold)) match
      case Left(e) => fail(s"expected a value, got: $e")
      case Right(t) => assertEquals(depthOf(t), PastThreshold + 1)
  }

  test("a genuinely deep document decodes correctly — no cap (remove-codecs-maxdepth)") {
    // this used to be refused outright at Codecs.maxDepth; once
    // JsonStrict.Reader.get trampolined past NativeThreshold, the cap
    // had no remaining job
    val n = 100000
    JsonStrict.read[Tree](jsonChain(n)) match
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
    assertEquals(JsonStrict.read[Order](Json.write(o)), Right(o))
    assertEquals(JsonStrict.read[Order](Json.write(o.copy(email = None))), Right(o.copy(email = None)))
  }

  test("an unknown field, deeply nested past the threshold, is still skipped") {
    final case class OnlyA(a: String)
    given Schema[OnlyA] = Schema.derived
    val text = "{\"a\":\"kept\",\"deep\":" + jsonChain(PastThreshold) + "}"
    JsonStrict.read[OnlyA](text) match
      case Left(e) => fail(s"expected the unknown field skipped, got: $e")
      case Right(v) => assertEquals(v, OnlyA("kept"))
  }

  test("errors past the threshold still refuse, not silently succeed") {
    final case class Bad(a: Int)
    given Schema[Bad] = Schema.derived
    assert(JsonStrict.read[Bad](jsonChain(PastThreshold)).isLeft)
  }

  test("Staged.strict agrees with the interpreted reader past the threshold") {
    val text = jsonChain(PastThreshold)
    assertEquals(Staged.strict[Tree].decode(text).map(depthOf), Right(PastThreshold + 1))
    assertEquals(Staged.strict[Tree].decode(text), JsonStrict.read[Tree](text))
  }
