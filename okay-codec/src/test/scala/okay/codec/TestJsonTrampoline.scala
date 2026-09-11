package okay.codec

/**
 * `Json.decode`'s threshold split (json-decode-threshold-trampoline,
 * specs/iterative-recursive-decode.md, root 2 of 2): native recursion
 * below `NativeThreshold`, `Cont.defer` past it, the same design
 * `Cbor.get` already carries (`TestCborTrampoline`). `Codecs.maxDepth`
 * — the wire-contract refusal `Json.isCut` used to enforce upstream,
 * at parse time — is gone (remove-codecs-maxdepth): a document this
 * deep now decodes through EITHER `Json.decode` directly or
 * `Json.read` (parse then decode), where it used to need building
 * directly to dodge the parser's own cut.
 */
class TestJsonTrampoline extends munit.FunSuite:

  // a genuinely deep document can legitimately take longer than
  // munit's 30s default under a loaded gate box (many suites running
  // at once) — this is correctness, not a performance test
  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def jsonChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  /** the same shape as a Json VALUE, built directly rather than
    * parsed — used where the test wants to exercise `Json.decode`
    * alone, independent of the parser */
  def jsonChainValue(n: Int): Json =
    var v: Json = Json.JObj(Vector("kids" -> Json.JArr(Vector.empty)))
    var i = 0
    while i < n do { v = Json.JObj(Vector("kids" -> Json.JArr(Vector(v)))); i += 1 }
    v

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d

  test("Json.decode is safe on a directly-built value at any depth") {
    val n = 200000
    Json.decode(summon[Schema[Tree]])(jsonChainValue(n)) match
      case Left(e) => fail(s"expected a value, got: $e")
      case Right(t) => assertEquals(depthOf(t), n + 1)
  }

  test("Json.read decodes a genuinely deep document correctly — no cap (remove-codecs-maxdepth)") {
    // this depth used to be refused at parse time (Json.isCut,
    // Codecs.maxDepth); with no cap, and Json.lossless's own O(n)
    // parse-quadratic-stack-length fix already landed, this is a
    // plain correct decode
    val n = 100000
    Json.read[Tree](jsonChain(n)) match
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
    assertEquals(Json.read[Order](Json.write(o)), Right(o))
    assertEquals(Json.read[Order](Json.write(o.copy(email = None))), Right(o.copy(email = None)))
  }

  test("errors past the threshold still refuse, not silently succeed") {
    final case class Bad(a: Int)
    given Schema[Bad] = Schema.derived
    val v = jsonChainValue(50)
    assert(Json.decode(summon[Schema[Bad]])(v).isLeft)
  }

  test("a damaged list element past the threshold is still skipped, not a fault") {
    // an element nested past NativeThreshold (a real Tree, decoded via
    // the trampoline) beside a DAMAGED element, both inside a List —
    // proves the trampoline's own list loop still applies the
    // skip-damage rule (input-depth-both-wires), not a plain re-throw
    // of the first Left it sees
    val goodDepth = jsonChainValue(30)
    val v = Json.JArr(Vector(goodDepth, Json.JErr("damaged")))
    Json.decode(summon[Schema[List[Tree]]])(v) match
      case Left(e) => fail(s"expected the damaged element skipped, got: $e")
      case Right(xs) => assertEquals(xs.map(depthOf), List(31))
  }
